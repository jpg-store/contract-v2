{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}

module Main where

import qualified Canonical.TestMinting as TestMinting
import Cardano.Api (displayError, writeFileTextEnvelope)
import Cardano.Transaction hiding (Value)
import qualified Cardano.Transaction as TxBuilder
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Identity
import Control.Monad.Managed
import Control.Monad.State
import Data.Aeson as Aeson
import Data.Aeson.Types as Aeson
import Data.Bifunctor
import Data.Foldable
import Data.List.Extra
import qualified Data.Map as Map
import Data.Maybe
import Data.String
import qualified Data.Text as Text
import Data.Traversable
import Env
import           Plutus.V1.Ledger.Crypto
import qualified Plutus.V1.Ledger.Value as Value
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.IO.Temp
import System.Process
import Test.Hspec
import qualified PlutusTx.Functor as P
import qualified PlutusTx.Prelude as P
import qualified PlutusTx.AssocMap as M

import Canonical.JpgStore.BulkPurchase
import Plutus.V1.Ledger.Credential
import AlwaysSucceed

deriving instance Show SwapAddress
deriving instance Eq SwapAddress
deriving instance Ord SwapAddress

expectedValueToValue :: ExpectedValue -> Value.Value
expectedValueToValue e = Value.Value $ P.fmap (\(_, x) -> P.fmap unWholeNumber x) e

valueToExpectedValue :: Value.Value -> ExpectedValue
valueToExpectedValue (Value.Value v) = P.fmap (\x -> (P.zero, P.fmap WholeNumber x)) v

lovelaceValueOf :: Integer -> Value.Value
lovelaceValueOf = Value.singleton "" ""

data Opts = Opts
  { optsSourceWalletAddressPath :: FilePath
  , optsSourceWalletSkeyPath :: FilePath
  , optsTestnetMagic :: Maybe Integer
  , optsWalletDir :: Maybe FilePath
  }

data Config = Config
  { cSourceWalletAddressPath :: FilePath
  , cSourceWalletSkeyPath :: FilePath
  , cWalletDir :: FilePath
  , cTestnetMagic :: Maybe Integer
  , cProtocolParams :: Maybe FilePath
  , cPlutusScript :: FilePath
  , cScriptAddr :: Address
  , cAlwaysSucceedScript :: FilePath
  , cAlwaysSucceedAddr :: Address
  }

data SwapAndDatum = SwapAndDatum
  { sadSwap :: Swap
  , sadValue :: Value.Value
  , sadDatumHash :: String
  }

data AlwaysSucceedDatumAndHash = AlwaysSucceedDatumAndHash
  { asdahDatum     :: Integer
  , asdahDatumHash :: String
  }

type Swaps = [SwapAndDatum]

data Wallet = Wallet
  { walletSkeyPath :: FilePath
  , walletAddr :: String
  , walletPkh :: String
  }
  deriving Show

data Wallets = Wallets
  { sellers :: [Wallet]
  , buyer :: Wallet
  , marketplace :: Wallet
  , royalties :: Wallet
  , scriptReference :: Wallet
  }
  deriving Show

data Policy = Policy
  { policyId :: String
  , policyFile :: FilePath
  }
  deriving Show

data Resources = Resources
  { rWallets :: Wallets
  , rScriptUtxos :: [UTxO]
  , rPolicies :: [Policy]
  }
  deriving Show

type SelectWallet = Wallets -> Wallet
type SelectPolicy = [Policy] -> Policy

seller1 :: SelectWallet
seller1 = (!! 0) . sellers

seller2 :: SelectWallet
seller2 = (!! 1) . sellers


policy1 :: SelectPolicy
policy1 = (!! 0)

policy2 :: SelectPolicy
policy2 = (!! 1)

policy3 :: SelectPolicy
policy3 = (!! 2)

policy4 :: SelectPolicy
policy4 = (!! 3)

runTests :: Config -> IO ()
runTests config@Config{..} = hspec $ aroundAllWith (createResources config) $ do
  describe "single offer" $ do
    it "can be for only the policy id" $ \Resources{..} -> do
      let
        thePolicy = head $ rPolicies

        thePolicyId :: Value.CurrencySymbol
        thePolicyId = fromString $ policyId thePolicy

        royaltyWallet = royalties rWallets
        marketplaceWallet = marketplace rWallets
        buyerWallet = buyer rWallets

        payouts =
          [ Payout (toSwapAddress $ fromString $ walletPkh buyerWallet) (M.singleton thePolicyId (Natural 1, M.empty))
          , Payout (toSwapAddress $ fromString $ walletPkh marketplaceWallet) (valueToExpectedValue $ lovelaceValueOf 1000000)
          , Payout (toSwapAddress $ fromString $ walletPkh royaltyWallet) (valueToExpectedValue $ lovelaceValueOf 1000000)
          ]
        swapDatum = Swap (fromString . walletPkh . buyer $ rWallets) payouts
        evalConfig = mempty
          { ecTestnet = cTestnetMagic
          , ecProtocolParams = cProtocolParams
          }

        buyerAddr = walletAddr buyerWallet

      void $ eval evalConfig $ do
        outputWithHash cScriptAddr "5000000 lovelace" swapDatum
        void $ selectInputs "7000000 lovelace " buyerAddr
        changeAddress buyerAddr
        void . balanceNonAdaAssets $ buyerAddr
        sign $ walletSkeyPath buyerWallet

      waitForNextBlock cTestnetMagic

      let
        sellerWallet = head $ sellers rWallets
        sellerAddr = walletAddr sellerWallet

      void $ evalMint config sellerWallet thePolicy "123456" 1
      waitForNextBlock cTestnetMagic

      let theAsset = show thePolicyId <> ".313233343536"

      void $ eval evalConfig $ do
        void $ output (walletAddr royaltyWallet) "1000000 lovelace"
        void $ output (walletAddr marketplaceWallet) "1000000 lovelace"
        void $ output buyerAddr $ fromString $ "2000000 lovelace + 1 " <> theAsset

        void $ selectInputs (fromString $ "2000000 lovelace + 1 " <> theAsset) sellerAddr

        void $ selectCollateralInput sellerAddr
        void $ balanceNonAdaAssets sellerAddr
        start <- currentSlot
        timerange start (start + 100)
        changeAddress sellerAddr
        sign $ walletSkeyPath sellerWallet

      waitForNextBlock cTestnetMagic

  aroundWith (createSwaps config [swapSpec seller1 policy1]) $ do
    it "can be purchased" $ \(resources, swaps) -> evalAccepts config resources swaps buyer

    it "can be cancelled by owner" $ \(resources, swaps) -> evalCancelSwaps config resources swaps seller1

    context "buyer counter offers" $ do
      aroundWith (createCounterOffer config) $ do
        it "seller can accept offer"
          $ \(resources, swaps, offer) -> evalAccept config resources (head swaps) offer



  describe "multiple offers" $ do
    context "from same seller" $ do
      aroundWith (createSwaps config [swapSpec seller1 policy1, swapSpec seller1 policy2]) $ do
        it "can be cancelled in bulk" $ \(resources, swaps) -> do
          evalCancelSwaps config resources swaps seller1

      context "for same offer" $ do
        aroundWith (createSwaps config [swapSpec seller1 policy1, swapSpec seller1 policy1]) $ do
          it "cannot be shorted" $ \(resources, swaps) -> do
            let
              buyerAddr = walletAddr . buyer $ rWallets resources
              evalConfig =
                EvalConfig
                  { ecOutputDir = Nothing
                  , ecTestnet = cTestnetMagic
                  , ecProtocolParams = cProtocolParams
                  , ecUseRequiredSigners = True
                  }
            void $ eval evalConfig $ do
              assets <-
                fmap (toTxValue . mconcat) . forScriptInputs config swaps $ \(s, v) utxo -> do
                  scriptInput utxo cPlutusScript s Accept
                  pure v

              void $ output buyerAddr (assets <> "1758582 lovelace")

              payoutTotal <- fmap mconcat $ for swaps $ \s -> fmap mconcat . for (sSwapPayouts $ sadSwap s) $ \Payout {..} ->
                let txValue = toTxValue $ expectedValueToValue pValue
                in txValue <$ output (lookupWalletAddr pAddress (rWallets resources)) txValue

              void $ selectInputs payoutTotal buyerAddr

              void $ selectCollateralInput buyerAddr
              void $ balanceNonAdaAssets buyerAddr
              start <- currentSlot
              timerange start (start + 100)
              changeAddress buyerAddr
              sign . walletSkeyPath . buyer $ rWallets resources

            waitForNextBlock cTestnetMagic

    context "from multiple sellers" $ do
      context "that have no expiration" $ do
        aroundWith (createSwaps config [swapSpec seller1 policy1, swapSpec seller2 policy3]) $ do
          it "cannot be cancelled in bulk" $ \(resources, swaps) -> do
            evalCancelSwaps config resources swaps seller1 `shouldThrow` isEvalException

          it "can be purchased in bulk" $ \(resources, swaps) -> do
            evalAccepts config resources swaps buyer

          it "Can't be purchased if another script is an input" $ \(resources, swaps) -> do
            evalAcceptsWithAlwaysSucceeds config resources swaps buyer `shouldThrow` isEvalException

        aroundWith (createSwaps config
          [ swapSpec seller1 policy1
          , swapSpec seller1 policy2
          , swapSpec seller1 policy3
          -- , swapSpec seller1 policy4
          -- , swapSpec seller2 policy1
          -- , swapSpec seller2 policy2
          -- , swapSpec seller2 policy3
          -- , swapSpec seller2 policy4
          ]) $ do
          it "can be really purchased in bulk upto 3" $ \(resources, swaps) -> do
            evalAccepts config resources swaps buyer

main :: IO ()
main = do
  opts <-
    Env.parse (header "integration-tests")
    $ Opts
    <$> var
          (str <=< nonempty)
          "SOURCE_WALLET_ADDRESS_PATH"
          (help "Path the address of the wallet to use for populating test wallets")
    <*> var
          (str <=< nonempty)
          "SOURCE_WALLET_SKEY_PATH"
          (help "Path the signing key of the wallet to use for populating test wallets")
    <*> optional (var auto "TESTNET_MAGIC" (help "Testnet magic"))
    <*> optional (var str "WALLET_DIR" (help "Directory for wallet files"))

  (callProcess "cardano-cli" . mappend ["query", "tip"] . toTestnetFlags . optsTestnetMagic $ opts) `catch` \e -> do
    let _e = e :: SomeException
    hPutStrLn stderr
      . mconcat
      $ [ "\n"
        , "Failed to query the network, check that:\n"
        , "* cardano-cli is on the PATH\n"
        , "* CARDANO_NODE_SOCKET_PATH is set correctly\n"
        , "* the cardano-node process is listening on the socket\n"
        ]
    exitFailure

  let
    withTempDir :: (FilePath -> IO a) -> IO a
    withTempDir = case optsWalletDir opts of
      Nothing -> withSystemTempDirectory "wallets"
      Just dir -> \run -> do
        createDirectoryIfMissing True dir
        run dir

    withConfig run =
      withPlutusFile (optsTestnetMagic opts)
        $ \plutusScript scriptAddr alwaysSucceedScript alwaysSucceedAddr -> withProtocolParams (optsTestnetMagic opts) $ \pp -> withTempDir $ \dir ->
            run $ Config
              { cSourceWalletSkeyPath = optsSourceWalletSkeyPath opts
              , cSourceWalletAddressPath = optsSourceWalletAddressPath opts
              , cWalletDir = dir
              , cProtocolParams = Just pp
              , cPlutusScript = plutusScript
              , cScriptAddr = scriptAddr
              , cTestnetMagic = optsTestnetMagic opts
              , cAlwaysSucceedScript = alwaysSucceedScript
              , cAlwaysSucceedAddr = alwaysSucceedAddr
              }

  withConfig runTests

isEvalException :: Selector EvalException
isEvalException = const True

toTxValue :: Value.Value -> TxBuilder.Value
toTxValue =
  mconcat
    . fmap
        (\(cs, tn, n) ->
          TxBuilder.Value . Map.singleton (show cs) . Map.singleton (encodedTokenName . Value.toString $ tn) $ n
        )
    . Value.flattenValue

allWallets :: Wallets -> [Wallet]
allWallets Wallets {..} = buyer : royalties : marketplace : sellers

lookupWallet :: SwapAddress -> Wallets -> Wallet
lookupWallet SwapAddress { sAddressCredential = PubKeyCredential pkh } =
  fromMaybe (error $ "couldn't find wallet for pkh " <> show pkh) . find ((show pkh ==) . walletPkh) . allWallets
lookupWallet _ = error "Bad address"

lookupWalletAddr :: SwapAddress -> Wallets -> String
lookupWalletAddr pkh = walletAddr . lookupWallet pkh

toSwapAddress :: PubKeyHash -> SwapAddress
toSwapAddress pkh = SwapAddress (PubKeyCredential pkh) Nothing

createScriptReference :: Config -> Resources -> IO ()
createScriptReference Config {..} Resources {..} = do
  let
    sellerWallet = seller1 rWallets
    sellerAddr = walletAddr sellerWallet

    evalConfig =
      EvalConfig { ecOutputDir = Nothing -- Just "temp/cbor"
                 , ecTestnet = cTestnetMagic
                 , ecProtocolParams = cProtocolParams
                 , ecUseRequiredSigners = False}

  void $ eval evalConfig $ do
    void $ outputWithScriptReference sellerAddr "50000000 lovelace" cPlutusScript

    _ <- selectCollateralInput sellerAddr
    void $ selectAllInputsAndSelfBalance sellerAddr
    changeAddress sellerAddr
    sign . walletSkeyPath $ sellerWallet

  waitForNextBlock cTestnetMagic


evalAccept :: Config -> Resources -> SwapAndDatum -> SwapAndDatum -> IO ()
evalAccept config@Config {..} Resources {..} theSwap offer = do
  let
    sellerPkh = sOwner . sadSwap $ theSwap
    sellerWallet = lookupWallet (toSwapAddress sellerPkh) rWallets
    sellerAddr = walletAddr sellerWallet
    buyerAddr = lookupWalletAddr (toSwapAddress . sOwner . sadSwap $ offer) rWallets

    asset = sadValue theSwap
    offerValue = sadValue offer
    sellerPayout = Payout (toSwapAddress sellerPkh) $ valueToExpectedValue offerValue
    payouts = [sellerPayout]

    evalConfig =
      EvalConfig { ecOutputDir = Nothing -- Just "temp/cbor"
                 , ecTestnet = cTestnetMagic
                 , ecProtocolParams = cProtocolParams
                 , ecUseRequiredSigners = True
                 }
  void $ eval evalConfig $ do
    void . forScriptInputs config [theSwap, offer] $ \(s, _) utxo -> do
      scriptInput utxo cPlutusScript s Accept

    void $ output buyerAddr (toTxValue asset <> "1758582 lovelace")

    for_ payouts $ \Payout {..} -> output (lookupWalletAddr pAddress rWallets) . toTxValue $
      expectedValueToValue pValue

    (cInput, _) <- selectCollateralInput sellerAddr
    input . iUtxo $ cInput
    void $ balanceNonAdaAssets sellerAddr
    start <- currentSlot
    timerange start (start + 100)
    changeAddress sellerAddr
    sign . walletSkeyPath $ sellerWallet

  waitForNextBlock cTestnetMagic

evalAccepts :: Config -> Resources -> [SwapAndDatum] -> SelectWallet -> IO ()
evalAccepts config@Config {..} Resources {..} swaps buyerW = do
  let
    buyerAddr = walletAddr . buyerW $ rWallets
    mergePayouts = fmap (uncurry Payout) . Map.toList . foldr (Map.unionWith unionExpectedValue) Map.empty . fmap
      (\Payout {..} -> Map.singleton pAddress pValue)
    evalConfig =
      EvalConfig { ecOutputDir = Nothing -- Just "temp/cbor"
                 , ecTestnet = cTestnetMagic
                 , ecProtocolParams = cProtocolParams
                 , ecUseRequiredSigners = True
                 }
  void $ eval evalConfig $ do
    swapUtxos <- makeScriptInputs config swaps
    let swapsAndReferenceUtxos = zip swapUtxos rScriptUtxos
    (payouts, assets) <-
      fmap (bimap (mergePayouts . mconcat) (toTxValue . mconcat) . unzip) . forM swapsAndReferenceUtxos $ \((s, v, inputUtxo), referenceUtxo) -> do
        scriptReferenceV2Input inputUtxo referenceUtxo s Accept Nothing
        pure (sSwapPayouts s, v)

    void $ output buyerAddr (assets <> "3000000 lovelace")

    payoutTotal <- fmap mconcat . for payouts $ \Payout {..} ->
      let txValue = toTxValue (expectedValueToValue pValue)
      in txValue <$ output (lookupWalletAddr pAddress rWallets) txValue

    void $ selectInputs payoutTotal buyerAddr

    void $ selectCollateralInput buyerAddr
    void $ balanceNonAdaAssets buyerAddr

    start <- currentSlot
    timerange start (start + 100)
    changeAddress buyerAddr
    sign . walletSkeyPath . buyerW $ rWallets

  waitForNextBlock cTestnetMagic

evalAcceptsWithAlwaysSucceeds :: Config -> Resources -> [SwapAndDatum] -> SelectWallet -> IO ()
evalAcceptsWithAlwaysSucceeds config@Config {..} Resources {..} swaps buyerW = do
  let
    buyerAddr = walletAddr . buyerW $ rWallets
    mergePayouts = fmap (uncurry Payout) . Map.toList . foldr (Map.unionWith unionExpectedValue) Map.empty . fmap
      (\Payout {..} -> Map.singleton pAddress pValue)
    evalConfig =
      EvalConfig { ecOutputDir = Nothing -- Just "temp/cbor"
                 , ecTestnet = cTestnetMagic
                 , ecProtocolParams = cProtocolParams
                 , ecUseRequiredSigners = True
                 }

  AlwaysSucceedDatumAndHash {..} <- lockAlwaysSucceed config $ buyer rWallets
  waitForNextBlock cTestnetMagic

  void $ eval evalConfig $ do
    lockUtxo <- liftIO . maybe (throwIO $ userError "firstScriptInput: no utxos") pure . listToMaybe =<<
      findScriptInputs cAlwaysSucceedAddr (UTxO_DatumHash asdahDatumHash)
    scriptInput lockUtxo cAlwaysSucceedScript asdahDatum (1 :: Integer)
    (payouts, assets) <-
      fmap (bimap (mergePayouts . mconcat) (toTxValue . mconcat) . unzip) . forScriptInputs config swaps $ \(s, v) utxo -> do
        scriptInput utxo cPlutusScript s Accept
        pure (sSwapPayouts s, v)

    void $ output buyerAddr (assets <> "1758582 lovelace")

    payoutTotal <- fmap mconcat . for payouts $ \Payout {..} ->
      let txValue = toTxValue (expectedValueToValue pValue)
      in txValue <$ output (lookupWalletAddr pAddress rWallets) txValue

    void $ selectInputs payoutTotal buyerAddr

    void $ selectCollateralInput buyerAddr
    void $ balanceNonAdaAssets buyerAddr
    start <- currentSlot
    timerange start (start + 100)
    changeAddress buyerAddr
    sign . walletSkeyPath . buyerW $ rWallets

  waitForNextBlock cTestnetMagic

evalCancelSwaps :: Config -> Resources -> [SwapAndDatum] -> SelectWallet -> IO ()
evalCancelSwaps config@Config {..} Resources {..} swaps canceller = do
  let
    Wallet {..} = canceller rWallets
    evalConfig = mempty { ecTestnet = cTestnetMagic, ecProtocolParams = cProtocolParams }

  void $ eval evalConfig $ do
    void $ forScriptInputs config swaps $ \(s, _) utxo -> scriptInput utxo cPlutusScript s Cancel
    (cin, _) <- selectCollateralInput walletAddr
    input . iUtxo $ cin
    void $ balanceNonAdaAssets walletAddr
    changeAddress walletAddr
    sign walletSkeyPath

  waitForNextBlock cTestnetMagic

-- The problem

makeScriptInputs :: Config -> [SwapAndDatum] -> Tx [(Swap, Value.Value, UTxO)]
makeScriptInputs Config {..} swaps = do
  let
    go :: [(Swap, Value.Value, UTxO)] -> [SwapAndDatum] -> Tx [(Swap, Value.Value, UTxO)]
    go acc = \case
      [] -> pure acc
      SwapAndDatum {..}:xs -> do
        utxos <- findScriptInputs cScriptAddr (UTxO_DatumHash sadDatumHash)
        -- we only take the first one in case there are multiple with the same datum from previous runs
        case filter (not . flip elem (map (\(_, _, a) -> a) acc)) utxos of
          [] ->
            liftIO
              . fail
              . mconcat
              $ ["no script inputs found for script address '", cScriptAddr, "' and datum hash '", sadDatumHash, "'"]
          y : _ -> go ((sadSwap, sadValue, y) : acc) xs
  go [] swaps

forScriptInputs :: Config -> [SwapAndDatum] -> ((Swap, Value.Value) -> UTxO -> Tx a) -> Tx [a]
forScriptInputs Config {..} swaps f = do
  let
    go :: [((Swap, Value.Value), UTxO)] -> [SwapAndDatum] -> Tx [((Swap, Value.Value), UTxO)]
    go acc = \case
      [] -> pure acc
      SwapAndDatum {..}:xs -> do
        utxos <- findScriptInputs cScriptAddr (UTxO_DatumHash sadDatumHash)
        -- we only take the first one in case there are multiple with the same datum from previous runs
        case filter (not . flip elem (map snd acc)) utxos of
          [] ->
            liftIO
              . fail
              . mconcat
              $ ["no script inputs found for script address '", cScriptAddr, "' and datum hash '", sadDatumHash, "'"]
          y : _ -> go (((sadSwap, sadValue), y) : acc) xs
  utxosAndSwaps <- go [] swaps
  forM utxosAndSwaps $ uncurry f

data SwapSpec = SwapSpec
  { specSeller :: SelectWallet
  , specPolicy :: SelectPolicy
  , specPayouts :: Wallets -> [Payout]
  }

stdPayouts :: SelectWallet -> Wallets -> [Payout]
stdPayouts seller wallets =
  [ Payout (toSwapAddress $ fromString . walletPkh . seller $ wallets) (valueToExpectedValue $ lovelaceValueOf 8000000)
  , Payout (toSwapAddress $ fromString . walletPkh . marketplace $ wallets) (valueToExpectedValue $ lovelaceValueOf 1000000)
  , Payout (toSwapAddress $ fromString . walletPkh . royalties $ wallets) (valueToExpectedValue $ lovelaceValueOf 1000000)
  ]

swapSpec :: SelectWallet -> SelectPolicy -> SwapSpec
swapSpec seller policy = SwapSpec seller policy (stdPayouts seller)

createSwaps :: Config -> [SwapSpec] -> ActionWith (Resources, Swaps) -> ActionWith Resources
createSwaps config specs runTest rs@Resources { rWallets, rPolicies } =
  let
    createSwap' SwapSpec {..} =
      createSwap config (specSeller rWallets) (specPolicy rPolicies) (specPayouts rWallets)
  in bracket (traverse createSwap' specs)
   -- (cleanup . (wallets, ))
          (\_ -> pure ()) (runTest . (rs, ))

lockAlwaysSucceed :: Config -> Wallet -> IO AlwaysSucceedDatumAndHash
lockAlwaysSucceed Config {..} Wallet {..} = do
  let
    evalConfig = mempty { ecTestnet = cTestnetMagic, ecProtocolParams = cProtocolParams }
    datum = 1 :: Integer

  datumHash <- hashDatum $ toCliJson datum

  void $ eval evalConfig $ do
    outputWithHash cAlwaysSucceedAddr "1500000 lovelace" datum
    void $ selectInputs "7000000 lovelace" walletAddr
    changeAddress walletAddr
    void . balanceNonAdaAssets $ walletAddr
    sign walletSkeyPath

  waitForNextBlock cTestnetMagic

  pure $ AlwaysSucceedDatumAndHash datum datumHash


createSwap :: Config -> Wallet -> Policy -> [Payout] -> IO SwapAndDatum
createSwap config@Config {..} wallet@Wallet {..} policy payouts = do
  let
    pValue = Value.singleton (fromString . policyId $ policy) "123456" 1
    swapDatum = Swap (fromString walletPkh) payouts
    evalConfig = mempty { ecTestnet = cTestnetMagic, ecProtocolParams = cProtocolParams }

  datumHash <- hashDatum . toCliJson $ swapDatum


  -- if there is an existing swap that is the same, reuse it
  -- existingScriptInput <- filter ((== Just datumHash) . utxoDatumHash) <$> queryUtxos walletAddr cTestnetMagic

  -- when (null existingScriptInput) $ do
  --do
  txValue <- evalMint config wallet policy "123456" 1
  waitForNextBlock cTestnetMagic

  void $ eval evalConfig $ do
    outputWithHash cScriptAddr ("5000000 lovelace" <> txValue) swapDatum
    void $ selectInputs "7000000 lovelace" walletAddr
    changeAddress walletAddr
    void . balanceNonAdaAssets $ walletAddr
    sign walletSkeyPath

  waitForNextBlock cTestnetMagic

  pure $ SwapAndDatum swapDatum pValue datumHash

createCounterOffer :: Config -> ActionWith (Resources, Swaps, SwapAndDatum) -> ActionWith (Resources, Swaps)
createCounterOffer Config {..} runTest (rs@Resources { rWallets }, swaps) = do
  let
    SwapAndDatum { sadValue } = head swaps
    Wallet {..} = buyer rWallets
    buyerPkh = fromString walletPkh
    buyerPayout = Payout (toSwapAddress buyerPkh) $ valueToExpectedValue sadValue
    offerValue = lovelaceValueOf 1500000
    txOfferValue = toTxValue offerValue
    offerDatum = Swap buyerPkh [buyerPayout]

    evalConfig =
      EvalConfig { ecOutputDir = Nothing -- Just "temp/cbor"
                 , ecTestnet = cTestnetMagic
                 , ecProtocolParams = cProtocolParams
                 , ecUseRequiredSigners = True
                 }

  datumHash <- hashDatum . toCliJson $ offerDatum

  void $ eval evalConfig $ do
    outputWithHash cScriptAddr txOfferValue offerDatum
    void $ selectInputs txOfferValue walletAddr
    changeAddress walletAddr
    void . balanceNonAdaAssets $ walletAddr
    sign walletSkeyPath

  waitForNextBlock cTestnetMagic

  runTest (rs, swaps, SwapAndDatum offerDatum offerValue datumHash)

encodedTokenName :: String -> String
encodedTokenName =
  Text.unpack
    . fromMaybe (error "unexpected tokenname serialization")
    . Aeson.parseMaybe (Aeson.withObject "TokenName" (.: "bytes"))
    . toCliJson
    . fromString @Value.TokenName

evalMint :: Config -> Wallet -> Policy -> String -> Integer -> IO TxBuilder.Value
evalMint Config { cTestnetMagic, cProtocolParams } Wallet { walletAddr, walletSkeyPath } policy token n = do
  let
    txValue = TxBuilder.Value . Map.singleton (policyId policy) . Map.singleton (encodedTokenName token) $ n
    evalConfig = mempty { ecTestnet = cTestnetMagic, ecProtocolParams = cProtocolParams }
  void $ eval evalConfig $ do
    mint txValue (policyFile policy) ([] @Int)

    void $ output walletAddr ("1758582 lovelace" <> txValue)
    (cInput, _) <- selectCollateralInput walletAddr
    input . iUtxo $ cInput

    changeAddress walletAddr
    void . balanceNonAdaAssets $ walletAddr

    sign walletSkeyPath

  pure txValue

createResources :: Config -> ActionWith Resources -> ActionWith ()
createResources config runTest _ =
  withWallets config $ \(wallets, utxos) -> withPolicies $ \policies -> do
      let resources = Resources wallets utxos $ policies
      runTest resources

withProtocolParams :: Maybe Integer -> (FilePath -> IO a) -> IO a
withProtocolParams testnetMagic runTest = withSystemTempFile "protocol-params.json" $ \fp fh -> do
  hClose fh
  callProcess "cardano-cli" (["query", "protocol-parameters", "--out-file", fp] <> toTestnetFlags testnetMagic)
  runTest fp

withPlutusFile :: Maybe Integer -> (FilePath -> Address -> FilePath -> Address -> IO a) -> IO a
withPlutusFile testnetMagic runTest = withSystemTempFile "swap.plutus" $ \fp fh -> withSystemTempFile "alwaysSucceeds.plutus" $ \fp' fh' -> do
  hClose fh
  hClose fh'
  writePlutusFile fp
  writeSucceedFile fp'
  scriptAddr <- readProcess
    "cardano-cli"
    (["address", "build", "--payment-script-file", fp] <> toTestnetFlags testnetMagic)
    mempty
  succeedScriptAddr <- readProcess
    "cardano-cli"
    (["address", "build", "--payment-script-file", fp'] <> toTestnetFlags testnetMagic)
    mempty
  putStrLn . mconcat $ ["Plutus script address: ", scriptAddr]
  putStrLn . mconcat $ ["Plutus script address: ", succeedScriptAddr]
  runTest fp scriptAddr fp' succeedScriptAddr

{- HLINT ignore "Avoid lambda" -}
withPolicies :: ([Policy] -> IO a) -> IO a
withPolicies = with (mapM (\n -> managed (withPolicy n)) [0 .. 3])

withPolicy :: Integer -> (Policy -> IO a) -> IO a
withPolicy n f = withSystemTempFile ("policy-" <> show n <> ".plutus") $ \fp fh -> do
  hClose fh
  r <- writeFileTextEnvelope fp Nothing $ TestMinting.mintingAsCbor n
  case r of
    Left err -> error $ displayError err
    Right () -> pure ()
  policyId <- trim <$> readProcess "cardano-cli" ["transaction", "policyid", "--script-file", fp] mempty
  f $ Policy policyId fp

withWallets :: Config -> ((Wallets, [UTxO]) -> IO c) -> IO c
withWallets config@Config {..} runTest =
  let
    createWallet' = createWallet config
    evalConfig = mempty { ecTestnet = cTestnetMagic
                        , ecProtocolParams = cProtocolParams
                        , ecUseRequiredSigners = False
                        }
  in bracket
    (do
      (wallets, newAddrs) <-
        flip runStateT []
        $ Wallets
        <$> traverse createWallet' ["seller1", "seller2"]
        <*> createWallet' "buyer"
        <*> createWallet' "marketplace"
        <*> createWallet' "royalties"
        <*> createWallet' "script-reference"

      let scriptReferenceAddr = walletAddr $ scriptReference $ wallets

      print newAddrs

      unless (null newAddrs) $ do
        void $ eval evalConfig $ do
          values <- traverse (\addr -> fmap oValue . output addr $ "100000000 lovelace") newAddrs

          let srcAddr = cSourceWalletAddressPath

          void $ selectInputs (mconcat values) srcAddr

          changeAddress srcAddr
          void $ balanceNonAdaAssets srcAddr

          sign cSourceWalletSkeyPath

        waitForNextBlock cTestnetMagic

      let scriptReferenceDeployTx i = do
            txId <- eval evalConfig $ do
              let srcAddr = cSourceWalletAddressPath
              scriptReferenceValues <- replicateM i $ outputWithScriptReference scriptReferenceAddr "35000000 lovelace" cPlutusScript

              void $ selectInputs (mconcat $ map oValue scriptReferenceValues) srcAddr

              changeAddress srcAddr

              sign cSourceWalletSkeyPath

            waitForNextBlock cTestnetMagic

            pure $ flip map [1..fromIntegral i] $ \utxoId -> UTxO
              { utxoIndex  = utxoId
              , utxoTx     = txId
              , utxoValue  = mempty
              , utxoDatum  = UTxO_NoDatum
              }

      scriptReferenceUtxos0 <- scriptReferenceDeployTx 2
      scriptReferenceUtxos1 <- scriptReferenceDeployTx 2
      scriptReferenceUtxos2 <- scriptReferenceDeployTx 2
      scriptReferenceUtxos3 <- scriptReferenceDeployTx 2

      let allScriptReferenceUtxos = concat
            [ scriptReferenceUtxos0
            , scriptReferenceUtxos1
            , scriptReferenceUtxos2
            , scriptReferenceUtxos3
            ]

      pure (wallets, allScriptReferenceUtxos)
    )
    (\_ -> pure ())
    runTest

createWallet :: Config -> String -> StateT [Address] IO Wallet
createWallet Config {..} name = do
  let
    vkeyFile = cWalletDir </> name <.> "vkey"
    skeyFile = cWalletDir </> name <.> "skey"
  exists <- liftIO $ doesFileExist vkeyFile
  liftIO $ unless exists $ do
    callProcess
      "cardano-cli"
      ["address", "key-gen", "--verification-key-file", vkeyFile, "--signing-key-file", skeyFile]
  addr <-
    let
      args = mconcat [["address", "build", "--payment-verification-key-file", vkeyFile], toTestnetFlags cTestnetMagic]
    in liftIO $ trim <$> readProcess "cardano-cli" args mempty

  unless exists $ modify (addr :)

  liftIO $ writeFile (cWalletDir </> name <.> "addr") addr
  pkh <-
    liftIO
    $ trim
    <$> readProcess "cardano-cli" ["address", "key-hash", "--payment-verification-key-file", vkeyFile] mempty

  pure $ Wallet skeyFile addr pkh
