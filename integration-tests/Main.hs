{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}

module Main where

import qualified Canonical.TestMinting as TestMinting
import Cardano.Api (displayError, writeFileTextEnvelope)
import Cardano.Transaction hiding (Value, Address)
import qualified Cardano.Transaction as TxBuilder
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Identity
import Control.Monad.Managed
import Control.Monad.State
import Data.Aeson as Aeson
import Data.Aeson.Types as Aeson
import Control.Lens ((^?))
import Data.Aeson.Lens (key, _String)
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
import           Plutus.V1.Ledger.Address
import           Plutus.V1.Ledger.Tx
import           Plutus.V1.Ledger.Bytes
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
import qualified Data.ByteString.Lazy.Char8 as BSLC

import Canonical.JpgStore.BulkPurchase
import qualified Canonical.JpgStore.NftMinter as NftMinter
import Canonical.Shared
import Plutus.V1.Ledger.Credential
import AlwaysSucceed

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
  , cScriptPkh :: String
  , cScriptAddr :: TxBuilder.Address
  , cAlwaysSucceedScript :: FilePath
  , cAlwaysSucceedAddr :: TxBuilder.Address
  , cNftTokenName :: String
  , cNftPolicyId :: String
  , cNftPlutus :: FilePath
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
  , configNft :: Wallet
  }
  deriving Show

data Policy = Policy
  { policyId :: String
  , policyFile :: FilePath
  }
  deriving Show

data Resources = Resources
  { rWallets :: Wallets
  , rPolicies :: [Policy]
  }
  deriving Show

data AllResources = AllResources
  { arResources :: Resources
  , arScriptUtxos :: [UTxO]
  , arConfigNft :: UTxO
  }

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

-- TODO
-- I need to mint the nft
-- add update the txes to use the nft

runTests :: Config -> AllResources -> IO ()
runTests config@Config{..} resources@AllResources
  { arResources = innerResources@Resources {..}
  , arConfigNft
  } = hspec $ do
  describe "single offer" $ do
    it "can be for only the policy id" $ do
      let
        thePolicy = head $ rPolicies

        thePolicyId :: Value.CurrencySymbol
        thePolicyId = fromString $ policyId thePolicy

        royaltyWallet = royalties rWallets
        marketplaceWallet = marketplace rWallets
        buyerWallet = buyer rWallets

        payouts =
          [ Payout (toSwapAddress $ fromString $ walletPkh buyerWallet) (M.singleton thePolicyId (Natural 1, M.empty))
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

        readOnlyInput arConfigNft

        start <- currentSlot
        timerange start (start + 100)
        changeAddress sellerAddr
        sign $ walletSkeyPath sellerWallet
        sign . walletSkeyPath $ marketplace rWallets

      waitForNextBlock cTestnetMagic

  before (createSwaps config innerResources [swapSpec seller1 policy1]) $ do
    it "can be purchased" $ \swaps -> evalAccepts config resources swaps 1_000_000 buyer

    it "can't be purchased if the fee is low"
      $ \swaps -> evalAcceptsLowFee config resources swaps buyer `shouldThrow` isEvalException

    it "can't be purchased without a marketplace signature"
      $ \swaps -> evalAcceptNoSignature config resources swaps buyer `shouldThrow` isEvalException

    it "can be cancelled by owner" $ \swaps -> evalCancelSwaps config resources swaps seller1

    it "can be updated by owner" $ \swaps -> evalCancelUpdateSwaps config resources swaps seller1

    it "can't be cancelled and sent anywhere" $ \swaps ->
      evalCancelWrongAddressSwaps config resources swaps seller1 `shouldThrow` isEvalException

    context "buyer counter offers" $ do
      aroundWith (createCounterOffer config innerResources) $ do
        it "seller can accept offer"
          $ \(swaps, offer) -> evalAccept config resources (head swaps) offer



  describe "multiple offers" $ do
    context "from same seller" $ do
      before (createSwaps config innerResources [swapSpec seller1 policy1, swapSpec seller1 policy2]) $ do
        it "can be cancelled in bulk" $ \swaps -> do
          evalCancelSwaps config resources swaps seller1

      context "for same offer" $ do
        before (createSwaps config innerResources [swapSpec seller1 policy1, swapSpec seller1 policy1]) $ do
          it "cannot be shorted" $ \swaps -> do
            let
              buyerAddr = walletAddr $ buyer rWallets
              marketplacePayout = Payout (toSwapAddress $ fromString . walletPkh . marketplace $ rWallets) (valueToExpectedValue $ lovelaceValueOf 2000000)
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

              payoutTotal <- fmap mconcat $ for (marketplacePayout : concatMap (sSwapPayouts . sadSwap) swaps) $ \Payout {..} ->
                let txValue = toTxValue $ expectedValueToValue pValue
                in txValue <$ output (lookupWalletAddr pAddress rWallets) txValue

              void $ selectInputs payoutTotal buyerAddr

              void $ selectCollateralInput buyerAddr
              void $ balanceNonAdaAssets buyerAddr

              readOnlyInput arConfigNft

              start <- currentSlot
              timerange start (start + 100)
              changeAddress buyerAddr
              sign . walletSkeyPath $ buyer rWallets
              sign . walletSkeyPath $ marketplace rWallets

            waitForNextBlock cTestnetMagic

    context "from multiple sellers" $ do
      context "that have no expiration" $ do
        before (createSwaps config innerResources [swapSpec seller1 policy1, swapSpec seller2 policy3]) $ do
          it "cannot be cancelled in bulk" $ \swaps -> do
            evalCancelSwaps config resources swaps seller1 `shouldThrow` isEvalException

          it "can be purchased in bulk" $ \swaps -> do
            evalAccepts config resources swaps 2_000_000 buyer

          it "Can't be purchased if another script is an input" $ \swaps -> do
            evalAcceptsWithAlwaysSucceeds config resources swaps buyer `shouldThrow` isEvalException

        before (createSwaps config innerResources
          [ swapSpec seller1 policy1
          , swapSpec seller1 policy2
          , swapSpec seller1 policy3
          , swapSpec seller1 policy4
          -- , swapSpec seller2 policy1
          -- , swapSpec seller2 policy2
          -- , swapSpec seller2 policy3
          -- , swapSpec seller2 policy4
          ]) $ do
          it "can be really purchased in bulk upto 3" $ \swaps -> do
            evalAccepts config resources swaps 4_000_000 buyer

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

    testnetMagic = optsTestnetMagic opts
    sourceWalletAddressPath = optsSourceWalletAddressPath opts
    sourceWalletSkeyPath = optsSourceWalletSkeyPath opts
    nftTokenName = "NFT"

  withTempDir $ \walletDir ->
    withProtocolParams testnetMagic $ \protocolParams -> do
      let mProtocolParams = Just protocolParams
      withResources testnetMagic mProtocolParams walletDir sourceWalletAddressPath sourceWalletSkeyPath $ \resources -> do
        withPlutusFile testnetMagic (rWallets resources) (fromString nftTokenName)
          $ \plutusScript scriptAddr alwaysSucceedScript alwaysSucceedAddr nftScript nftPolicyId scriptPkh -> do
              scriptReferences <- createScriptReferences resources plutusScript sourceWalletAddressPath sourceWalletSkeyPath testnetMagic mProtocolParams


              let
                config = Config
                  { cSourceWalletSkeyPath = sourceWalletSkeyPath
                  , cSourceWalletAddressPath = sourceWalletAddressPath
                  , cWalletDir = walletDir
                  , cProtocolParams = mProtocolParams
                  , cPlutusScript = plutusScript
                  , cScriptPkh = scriptPkh
                  , cScriptAddr = scriptAddr
                  , cNftTokenName = nftTokenName
                  , cNftPolicyId = nftPolicyId
                  , cNftPlutus = nftScript
                  , cTestnetMagic = testnetMagic
                  , cAlwaysSucceedScript = alwaysSucceedScript
                  , cAlwaysSucceedAddr = alwaysSucceedAddr
                  }

              nftUTxO <- mintConfigNft config (configNft $ rWallets resources) (marketplace $ rWallets resources) (Policy nftPolicyId nftScript) nftTokenName 1

              let
                allResources = AllResources
                  { arResources = resources
                  , arScriptUtxos = scriptReferences
                  , arConfigNft = nftUTxO
                  }

              runTests config allResources

createScriptReferences :: Resources
                        -> [Char]
                        -> [Char]
                        -> [Char]
                        -> Maybe Integer
                        -> Maybe FilePath
                        -> IO [UTxO]
createScriptReferences Resources {..} plutusScript sourceWalletAddressPath sourceWalletSkeyPath testnetMagic protocolParams = do
  let scriptReferenceAddr = walletAddr $ scriptReference rWallets

      evalConfig = mempty { ecTestnet = testnetMagic
                    , ecProtocolParams = protocolParams
                    , ecUseRequiredSigners = False
                    }

      scriptReferenceDeployTx i = do
        txId <- eval evalConfig $ do
          let srcAddr = sourceWalletAddressPath
          scriptReferenceValues <- replicateM i $ outputWithScriptReference scriptReferenceAddr "40000000 lovelace" plutusScript

          void $ selectInputs (mconcat $ map oValue scriptReferenceValues) srcAddr

          changeAddress srcAddr

          sign sourceWalletSkeyPath

        waitForNextBlock testnetMagic

        pure $ flip map [1..fromIntegral i] $ \utxoId -> UTxO
          { utxoIndex  = utxoId
          , utxoTx     = txId
          , utxoValue  = mempty
          , utxoDatum  = UTxO_NoDatum
          }

  concat
    <$> replicateM 8 (scriptReferenceDeployTx 1)

mintConfigNft :: Config -> Wallet -> Wallet -> Policy -> String -> Integer -> IO UTxO
mintConfigNft Config { cTestnetMagic, cProtocolParams, cScriptPkh } Wallet { walletAddr, walletSkeyPath } Wallet { walletPkh = marketplacePkh } policy token n = do
  let
    txValue = TxBuilder.Value . Map.singleton (policyId policy) . Map.singleton (encodedTokenName token) $ n
    outputValue = "3000000 lovelace" <> txValue
    evalConfig = mempty { ecTestnet = cTestnetMagic, ecProtocolParams = cProtocolParams }

  void $ eval evalConfig $ do

    let dynamicConfig = NftMinter.SwapDynamicConfig
          { sdcValidOutputAddresses = NftMinter.NonEmptyAddress (Address (ScriptCredential (fromString cScriptPkh)) Nothing) []
          -- ^ change this to addresses
          , sdcMarketplaceAddresses = NftMinter.NonEmptyPaymentAddress (NftMinter.PaymentAddress (fromString marketplacePkh) Nothing) []
          }

    mint txValue (policyFile policy) ([] @Int)

    void $ outputWithInlineDatum walletAddr outputValue dynamicConfig
    (cInput, _) <- selectCollateralInput walletAddr
    input . iUtxo $ cInput

    changeAddress walletAddr
    void . balanceNonAdaAssets $ walletAddr

    sign walletSkeyPath

  waitForNextBlock cTestnetMagic

  utxos <- queryUtxos walletAddr cTestnetMagic

  case filter ((outputValue ==) . utxoValue) utxos of
    []  -> throwIO $ userError "failed to find nft utxo"
    [x] -> pure x
    _   -> throwIO $ userError "too many nft utxos"

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

lookupWallet :: Address -> Wallets -> Wallet
lookupWallet Address { addressCredential = PubKeyCredential pkh } =
  fromMaybe (error $ "couldn't find wallet for pkh " <> show pkh) . find ((show pkh ==) . walletPkh) . allWallets
lookupWallet _ = error "Bad address"

lookupWalletAddr :: Address -> Wallets -> String
lookupWalletAddr pkh = walletAddr . lookupWallet pkh

toSwapAddress :: PubKeyHash -> Address
toSwapAddress pkh = Address (PubKeyCredential pkh) Nothing

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


evalAccept :: Config -> AllResources -> SwapAndDatum -> SwapAndDatum -> IO ()
evalAccept config@Config {..} AllResources
  { arResources = Resources {..}
  , arConfigNft
  } theSwap offer = do
  let
    marketplacePayout = Payout (toSwapAddress $ fromString . walletPkh . marketplace $ rWallets) (valueToExpectedValue $ lovelaceValueOf 1000000)
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

    for_ (marketplacePayout : payouts) $ \Payout {..} -> output (lookupWalletAddr pAddress rWallets) . toTxValue $
      expectedValueToValue pValue

    (cInput, _) <- selectCollateralInput sellerAddr
    input . iUtxo $ cInput

    readOnlyInput arConfigNft

    void $ balanceNonAdaAssets sellerAddr
    start <- currentSlot
    timerange start (start + 100)
    changeAddress sellerAddr
    sign . walletSkeyPath $ sellerWallet
    sign . walletSkeyPath . marketplace $ rWallets

  waitForNextBlock cTestnetMagic

evalAcceptsLowFee :: Config -> AllResources -> [SwapAndDatum] -> SelectWallet -> IO ()
evalAcceptsLowFee config@Config {..} AllResources
  { arResources = Resources {..}
  , arScriptUtxos
  , arConfigNft
  } swaps buyerW = do
  let
    -- this is wrong. I need to calculate one per swap
    marketplacePayout = Payout (toSwapAddress $ fromString . walletPkh . marketplace $ rWallets) (valueToExpectedValue $ lovelaceValueOf 900000)
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
    let swapsAndReferenceUtxos = zip swapUtxos arScriptUtxos
    (payouts, assets) <-
      fmap (bimap (mergePayouts . mconcat) (toTxValue . mconcat) . unzip) . forM swapsAndReferenceUtxos $ \((s, v, inputUtxo), referenceUtxo) -> do
        scriptReferenceV2Input inputUtxo referenceUtxo s Accept Nothing
        pure (sSwapPayouts s, v)

    void $ output buyerAddr (assets <> "3000000 lovelace")

    payoutTotal <- fmap mconcat . for (marketplacePayout : payouts) $ \Payout {..} ->
      let txValue = toTxValue (expectedValueToValue pValue)
      in txValue <$ output (lookupWalletAddr pAddress rWallets) txValue

    void $ selectInputs payoutTotal buyerAddr

    readOnlyInput arConfigNft

    void $ selectCollateralInput buyerAddr
    void $ balanceNonAdaAssets buyerAddr

    start <- currentSlot
    timerange start (start + 100)
    changeAddress buyerAddr
    sign . walletSkeyPath . buyerW $ rWallets
    sign . walletSkeyPath . marketplace $ rWallets

  waitForNextBlock cTestnetMagic

evalAccepts :: Config -> AllResources -> [SwapAndDatum] -> Integer -> SelectWallet -> IO ()
evalAccepts config@Config {..} AllResources
  { arResources = Resources {..}
  , arScriptUtxos
  , arConfigNft
  } swaps marketplaceAmount buyerW = do
  let
    -- this is wrong. I need to calculate one per swap
    marketplacePayout = Payout (toSwapAddress $ fromString . walletPkh . marketplace $ rWallets) (valueToExpectedValue $ lovelaceValueOf marketplaceAmount)
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
    let swapsAndReferenceUtxos = zip swapUtxos arScriptUtxos
    (payouts, assets) <-
      fmap (bimap (mergePayouts . mconcat) (toTxValue . mconcat) . unzip) . forM swapsAndReferenceUtxos $ \((s, v, inputUtxo), referenceUtxo) -> do
        scriptReferenceV2Input inputUtxo referenceUtxo s Accept Nothing
        pure (sSwapPayouts s, v)

    void $ output buyerAddr (assets <> "3000000 lovelace")

    payoutTotal <- fmap mconcat . for (marketplacePayout : payouts) $ \Payout {..} ->
      let txValue = toTxValue (expectedValueToValue pValue)
      in txValue <$ output (lookupWalletAddr pAddress rWallets) txValue

    void $ selectInputs payoutTotal buyerAddr

    readOnlyInput arConfigNft

    void $ selectCollateralInput buyerAddr
    void $ balanceNonAdaAssets buyerAddr

    start <- currentSlot
    timerange start (start + 100)
    changeAddress buyerAddr
    sign . walletSkeyPath . buyerW $ rWallets
    sign . walletSkeyPath . marketplace $ rWallets

  waitForNextBlock cTestnetMagic

evalAcceptNoSignature :: Config -> AllResources -> [SwapAndDatum] -> SelectWallet -> IO ()
evalAcceptNoSignature config@Config {..} AllResources
  { arResources = Resources {..}
  , arScriptUtxos
  , arConfigNft
  } swaps buyerW = do
  let
    marketplacePayout = Payout (toSwapAddress $ fromString . walletPkh . marketplace $ rWallets) (valueToExpectedValue $ lovelaceValueOf 1000000)
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
    let swapsAndReferenceUtxos = zip swapUtxos arScriptUtxos
    (payouts, assets) <-
      fmap (bimap (mergePayouts . mconcat) (toTxValue . mconcat) . unzip) . forM swapsAndReferenceUtxos $ \((s, v, inputUtxo), referenceUtxo) -> do
        scriptReferenceV2Input inputUtxo referenceUtxo s Accept Nothing
        pure (sSwapPayouts s, v)

    void $ output buyerAddr (assets <> "3000000 lovelace")

    payoutTotal <- fmap mconcat . for (marketplacePayout : payouts) $ \Payout {..} ->
      let txValue = toTxValue (expectedValueToValue pValue)
      in txValue <$ output (lookupWalletAddr pAddress rWallets) txValue

    void $ selectInputs payoutTotal buyerAddr

    readOnlyInput arConfigNft

    void $ selectCollateralInput buyerAddr
    void $ balanceNonAdaAssets buyerAddr

    start <- currentSlot
    timerange start (start + 100)
    changeAddress buyerAddr
    sign . walletSkeyPath . buyerW $ rWallets

  waitForNextBlock cTestnetMagic

evalAcceptsWithAlwaysSucceeds :: Config -> AllResources -> [SwapAndDatum] -> SelectWallet -> IO ()
evalAcceptsWithAlwaysSucceeds config@Config {..} AllResources
  { arResources = Resources {..}
  , arConfigNft
  } swaps buyerW = do
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

    readOnlyInput arConfigNft

    void $ selectCollateralInput buyerAddr
    void $ balanceNonAdaAssets buyerAddr
    start <- currentSlot
    timerange start (start + 100)
    changeAddress buyerAddr
    sign . walletSkeyPath . buyerW $ rWallets
    sign . walletSkeyPath . marketplace $ rWallets

  waitForNextBlock cTestnetMagic

evalCancelSwaps :: Config -> AllResources -> [SwapAndDatum] -> SelectWallet -> IO ()
evalCancelSwaps config@Config {..} AllResources
  { arResources = Resources {..}
  , arConfigNft
  } swaps canceller = do
  let
    Wallet {..} = canceller rWallets
    evalConfig = mempty { ecTestnet = cTestnetMagic
                        , ecProtocolParams = cProtocolParams
                        , ecUseRequiredSigners = True
                        }

  void $ eval evalConfig $ do
    void $ forScriptInputs config swaps $ \(s, _) utxo -> scriptInput utxo cPlutusScript s Cancel
    (cin, _) <- selectCollateralInput walletAddr
    input . iUtxo $ cin

    readOnlyInput arConfigNft
    void $ output walletAddr ("15000000 lovelace")
    void $ balanceNonAdaAssets walletAddr
    changeAddress walletAddr
    sign walletSkeyPath

  waitForNextBlock cTestnetMagic

evalCancelWrongAddressSwaps :: Config -> AllResources -> [SwapAndDatum] -> SelectWallet -> IO ()
evalCancelWrongAddressSwaps config@Config {..} AllResources
  { arResources = Resources {..}
  , arConfigNft
  } swaps canceller = do
  let
    Wallet {walletAddr = theWalletAddr, walletSkeyPath} = canceller rWallets
    evalConfig = mempty { ecTestnet = cTestnetMagic
                        , ecProtocolParams = cProtocolParams
                        , ecUseRequiredSigners = True
                        }

    outputAddr = walletAddr $ marketplace rWallets

  void $ eval evalConfig $ do
    void $ forScriptInputs config swaps $ \(s, _) utxo -> scriptInput utxo cPlutusScript s Cancel
    (cin, _) <- selectCollateralInput theWalletAddr
    input . iUtxo $ cin

    readOnlyInput arConfigNft
    void $ output outputAddr ("15000000 lovelace")
    void $ balanceNonAdaAssets outputAddr
    changeAddress outputAddr
    sign walletSkeyPath

  waitForNextBlock cTestnetMagic

evalCancelUpdateSwaps :: Config -> AllResources -> [SwapAndDatum] -> SelectWallet -> IO ()
evalCancelUpdateSwaps config@Config {..} AllResources
  { arResources = Resources {..}
  , arConfigNft
  } swaps canceller = do
  let
    Wallet {..} = canceller rWallets
    evalConfig = mempty { ecTestnet = cTestnetMagic
                        , ecProtocolParams = cProtocolParams
                        , ecUseRequiredSigners = True
                        }

  void $ eval evalConfig $ do
    void $ forScriptInputs config swaps $ \(s, _) utxo -> do
      scriptInput utxo cPlutusScript s Cancel
      outputWithHash cScriptAddr (utxoValue utxo) s

    (cin, _) <- selectCollateralInput walletAddr
    input . iUtxo $ cin

    readOnlyInput arConfigNft

    void $ balanceNonAdaAssets walletAddr
    changeAddress walletAddr
    sign walletSkeyPath

  waitForNextBlock cTestnetMagic

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
  , Payout (toSwapAddress $ fromString . walletPkh . royalties $ wallets) (valueToExpectedValue $ lovelaceValueOf 1000000)
  ]

swapSpec :: SelectWallet -> SelectPolicy -> SwapSpec
swapSpec seller policy = SwapSpec seller policy (stdPayouts seller)

createSwaps :: Config -> Resources -> [SwapSpec] -> IO [SwapAndDatum]
createSwaps config Resources { rWallets, rPolicies } specs =
  forM specs $ \SwapSpec {..} ->
    createSwap config (specSeller rWallets) (specPolicy rPolicies) (specPayouts rWallets)


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

createCounterOffer :: Config -> Resources -> ActionWith (Swaps, SwapAndDatum) -> ActionWith Swaps
createCounterOffer Config {..} Resources { rWallets } runTest swaps = do
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

  runTest (swaps, SwapAndDatum offerDatum offerValue datumHash)

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

withResources :: Maybe Integer -> Maybe FilePath -> FilePath -> FilePath -> FilePath -> (Resources -> IO r)-> IO r
withResources testnetMagic protocolParams walletDir sourceWalletAddressPath sourceWalletSkeyPath cont =
  withPolicies $ \policies -> do
    wallets <- createWallets testnetMagic protocolParams walletDir sourceWalletAddressPath sourceWalletSkeyPath
    let resources = Resources wallets $ policies
    cont resources

withProtocolParams :: Maybe Integer -> (FilePath -> IO a) -> IO a
withProtocolParams testnetMagic runTest = withSystemTempFile "protocol-params.json" $ \fp fh -> do
  hClose fh
  callProcess "cardano-cli" (["query", "protocol-parameters", "--out-file", fp] <> toTestnetFlags testnetMagic)
  runTest fp

withPlutusFile :: Maybe Integer -> Wallets -> Value.TokenName -> (FilePath -> TxBuilder.Address -> FilePath -> TxBuilder.Address -> FilePath -> String -> String -> IO a) -> IO a
withPlutusFile testnetMagic wallets nftTokenName runTest = withSystemTempFile "nft.plutus" $ \nftFp nftFh -> withSystemTempFile "nftPolicyId.txt" $ \policyIdFp policyIdFh -> withSystemTempFile "swap.plutus" $ \fp fh -> withSystemTempFile "alwaysSucceeds.plutus" $ \fp' fh' -> do
  mapM_ hClose [fh, fh', nftFh, policyIdFh]
  let
    theWalletAddr = walletAddr $ configNft wallets

  sellerUtxos <- queryUtxos theWalletAddr testnetMagic
  nftUtxo <- case sellerUtxos of
    x : _ -> pure $ TxOutRef (TxId $ getLedgerBytes $ fromString $ utxoTx x) (utxoIndex x)
    _ -> throwIO $ userError "no utxos!"

  let
    nftConfig = NftMinter.NftConfig
      { ncInitialUtxo = nftUtxo
      , ncTokenName   = nftTokenName
      }

  writeFileTextEnvelope nftFp Nothing (NftMinter.nftMinter nftConfig) >>= \case
      Left err -> print $ displayError err
      Right () -> putStrLn $ "wrote validator to file " ++ nftFp

  let theNftPolicyId = NftMinter.nftMinterPolicyId nftConfig

  writeFile policyIdFp $ show theNftPolicyId

  let swapConfig = SwapConfig
        { scMarketplaceFee    = 111
        , scConfigNftPolicyId = theNftPolicyId
        , scConfigNftTokenName = nftTokenName
        }

  writePlutusFile swapConfig fp

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

  resultJson <-
    trim <$> readProcess "cardano-address" ["address", "inspect"] scriptAddr

  scriptPkh <- case Aeson.decode (BSLC.pack resultJson) of
    Nothing -> throwIO $ userError "failed to decode cardano-address json"
    Just (v :: Value)  -> case v ^? key "spending_shared_hash" . _String of
      Nothing -> throwIO $ userError "failed to find spending_shared_hash key"
      Just ht -> pure $ Text.unpack ht

  putStrLn . mconcat $ ["Plutus script pkh: ", scriptPkh]

  runTest fp scriptAddr fp' succeedScriptAddr nftFp (show theNftPolicyId) scriptPkh

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

createWallets :: Maybe Integer -> Maybe FilePath -> FilePath -> FilePath -> FilePath -> IO Wallets
createWallets testnetMagic protocolParams walletDir sourceWalletAddressPath sourceWalletSkeyPath = do
  let
    createWallet' = createWallet testnetMagic walletDir
    evalConfig = mempty { ecTestnet = testnetMagic
                        , ecProtocolParams = protocolParams
                        , ecUseRequiredSigners = False
                        }

  (wallets, newAddrs) <-
    flip runStateT []
    $ Wallets
    <$> traverse createWallet' ["seller1", "seller2"]
    <*> createWallet' "buyer"
    <*> createWallet' "marketplace"
    <*> createWallet' "royalties"
    <*> createWallet' "script-reference"
    <*> createWallet' "config-nft"

  print newAddrs

  unless (null newAddrs) $ do
    void $ eval evalConfig $ do
      values <- traverse (\addr -> fmap oValue . output addr $ "100000000 lovelace") newAddrs

      let srcAddr = sourceWalletAddressPath

      void $ selectInputs (mconcat values) srcAddr

      changeAddress srcAddr
      void $ balanceNonAdaAssets srcAddr

      sign sourceWalletSkeyPath

    waitForNextBlock testnetMagic

  pure wallets


createWallet :: Maybe Integer -> FilePath -> String -> StateT [TxBuilder.Address] IO Wallet
createWallet testnetMagic walletDir name = do
  let
    vkeyFile = walletDir </> name <.> "vkey"
    skeyFile = walletDir </> name <.> "skey"
  exists <- liftIO $ doesFileExist vkeyFile
  liftIO $ unless exists $ do
    callProcess
      "cardano-cli"
      ["address", "key-gen", "--verification-key-file", vkeyFile, "--signing-key-file", skeyFile]
  addr <-
    let
      args = mconcat [["address", "build", "--payment-verification-key-file", vkeyFile], toTestnetFlags testnetMagic]
    in liftIO $ trim <$> readProcess "cardano-cli" args mempty

  unless exists $ modify (addr :)

  liftIO $ writeFile (walletDir </> name <.> "addr") addr
  pkh <-
    liftIO
    $ trim
    <$> readProcess "cardano-cli" ["address", "key-hash", "--payment-verification-key-file", vkeyFile] mempty

  pure $ Wallet skeyFile addr pkh
