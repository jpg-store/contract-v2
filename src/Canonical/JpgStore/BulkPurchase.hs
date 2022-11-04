{-# LANGUAGE NoImplicitPrelude #-}
module Canonical.JpgStore.BulkPurchase
  ( ExpectedValue
  , Natural(..)
  , Payout(..)
  , Action(..)
  , Swap(..)
  , WholeNumber(..)
  , swap
  , writePlutusFile
  , unionExpectedValue
  , satisfyExpectations
  ) where

{- HLINT ignore module "Eta reduce" -}
{- HLINT ignore module "Avoid lambda" -}

import Canonical.Shared
import qualified Cardano.Api as Api
import           Cardano.Api.Shelley (PlutusScript(..), PlutusScriptV2)
import           Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as SBS
import           Plutus.V1.Ledger.Address
import           Plutus.V1.Ledger.Crypto
import           Plutus.V2.Ledger.Contexts
import           Plutus.V1.Ledger.Scripts
import           Plutus.V2.Ledger.Tx
import           Plutus.V1.Ledger.Credential
import           Plutus.V1.Ledger.Value
import           PlutusTx
import qualified PlutusTx.AssocMap as M
import           PlutusTx.AssocMap (Map)
import           PlutusTx.Prelude
import           Prelude (IO, print, putStrLn)
import           System.FilePath
import           PlutusTx.These
import qualified Plutonomy

#include "../DebugUtilities.h"

newtype WholeNumber = WholeNumber { unWholeNumber :: Integer }
  deriving newtype (Eq, AdditiveSemigroup, Ord, ToData)

mkWholeNumber :: Integer -> WholeNumber
mkWholeNumber i
  | i < 1 = TRACE_ERROR("WholeNumber is less than 1", "-8")
  | otherwise = WholeNumber i

instance FromData WholeNumber where
  fromBuiltinData x = case fromBuiltinData x of
    Nothing -> Nothing
    Just i -> if i > 0
      then Just (WholeNumber i)
      else Nothing

instance UnsafeFromData WholeNumber where
  unsafeFromBuiltinData x =
    let i = unsafeFromBuiltinData x
    in if i > 0
      then WholeNumber i
      else TRACE_ERROR("WholeNumber is less than 1", "-2")

newtype Natural = Natural Integer
  deriving newtype (Eq, AdditiveSemigroup, AdditiveMonoid, Ord, ToData)

instance FromData Natural where
  fromBuiltinData x = case fromBuiltinData x of
    Nothing -> Nothing
    Just i -> if i > (-1)
      then Just (Natural i)
      else Nothing

instance UnsafeFromData Natural where
  unsafeFromBuiltinData x =
    let i = unsafeFromBuiltinData x
    in if i > (-1)
      then Natural i
      else TRACE_ERROR("Natural is less than 0", "-3")

data SwapTxOut = SwapTxOut
  { sTxOutAddress         :: Address
  , sTxOutValue           :: Value
  , sTxOutDatum           :: OutputDatum
  , sTxOutReferenceScript :: BuiltinData
  }

data SwapTxInInfo = SwapTxInInfo
  { sTxInInfoOutRef :: TxOutRef
  , sTxInInfoResolved :: SwapTxOut
  }

data SwapTxInfo = SwapTxInfo
  { sTxInfoReferenceInputs :: [SwapTxInInfo]
  , sTxInfoOutputs :: [SwapTxOut]
  , sTxInfoSignatories :: [PubKeyHash]
  , sTxInfoData :: Map DatumHash Datum
  }

data PartialSwapAddress = PartialSwapAddress
  { psAddressCredential        :: Credential
  , psAddressStakingCredential :: BuiltinData
  }

data PartialSwapTxOut = PartialSwapTxOut
  { psTxOutAddress         :: PartialSwapAddress
  , psTxOutValue           :: BuiltinData
  , psTxOutDatum           :: BuiltinData
  , psTxOutReferenceScript :: BuiltinData
  }

data PartialSwapTxInInfo = PartialSwapTxInInfo
  { psTxInInfoOutRef   :: BuiltinData
  , psTxInInfoResolved :: PartialSwapTxOut
  }

data PartialSwapTxInfo = PartialSwapTxInfo
  { pTxInfoInputs :: [PartialSwapTxInInfo]
  , pTxInfoReferenceInputs    :: BuiltinData
  , pTxInfoOutputs :: BuiltinData
  , pTxInfoFee :: BuiltinData
  , pTxInfoMint :: BuiltinData
  , pTxInfoDCert :: BuiltinData
  , pTxInfoWdrl :: BuiltinData
  , pTxInfoValidRange :: BuiltinData
  , pTxInfoSignatories :: BuiltinData
  , pTxInfoRedeemers          :: BuiltinData
  , pTxInfoData :: BuiltinData
  , pTxInfoId :: BuiltinData
  }

convertToSwapInputs :: PartialSwapTxInInfo -> SwapTxInInfo
convertToSwapInputs PartialSwapTxInInfo {..} = SwapTxInInfo
  { sTxInInfoOutRef = unsafeFromBuiltinData psTxInInfoOutRef
  , sTxInInfoResolved = convertToSwapOutput psTxInInfoResolved
  }

convertToSwapOutput :: PartialSwapTxOut -> SwapTxOut
convertToSwapOutput PartialSwapTxOut {..} = SwapTxOut
  { sTxOutAddress = convertToSwapAddress psTxOutAddress
  , sTxOutValue = unsafeFromBuiltinData psTxOutValue
  , sTxOutDatum = unsafeFromBuiltinData psTxOutDatum
  , sTxOutReferenceScript = psTxOutReferenceScript
  }

convertToSwapAddress :: PartialSwapAddress -> Address
convertToSwapAddress PartialSwapAddress {..} = Address
  { addressCredential = psAddressCredential
  , addressStakingCredential = unsafeFromBuiltinData psAddressStakingCredential
  }

convertToSwapTxInfo :: PartialSwapTxInfo -> SwapTxInfo
convertToSwapTxInfo PartialSwapTxInfo {..} = SwapTxInfo
  { sTxInfoReferenceInputs = unsafeFromBuiltinData pTxInfoReferenceInputs
  , sTxInfoOutputs         = unsafeFromBuiltinData pTxInfoOutputs
  , sTxInfoSignatories     = unsafeFromBuiltinData pTxInfoSignatories
  , sTxInfoData            = unsafeFromBuiltinData pTxInfoData
  }

{- HLINT ignore SwapScriptPurpose -}
data SwapScriptPurpose
    = ASpending TxOutRef

data SwapScriptContext = SwapScriptContext
  { sScriptContextTxInfo :: PartialSwapTxInfo
  , sScriptContextPurpose :: SwapScriptPurpose
  }

type ExpectedValue = M.Map CurrencySymbol (Natural, M.Map TokenName WholeNumber)

expectedLovelaces :: ExpectedValue -> Integer
expectedLovelaces cm = case M.lookup adaSymbol cm of
  Nothing -> 0
  Just (Natural cc, tm) -> case M.lookup adaToken tm of
    Nothing -> cc
    Just (WholeNumber tc) -> tc + cc

lovelaces :: Value -> Integer
lovelaces (Value cm) = case M.lookup adaSymbol cm of
  Nothing -> 0
  Just tm -> case M.lookup adaToken tm of
    Nothing -> 0
    Just c  -> c

{-# INLINABLE unionExpectedValue #-}
unionExpectedValue :: ExpectedValue -> ExpectedValue -> ExpectedValue
unionExpectedValue l r =
    let
        combined = M.union l r

        innerUnThese :: These WholeNumber WholeNumber -> WholeNumber
        innerUnThese k = case k of
            This a    -> a
            That b    -> b
            These a b -> a + b

        unThese :: These
                    (Natural, Map TokenName WholeNumber)
                    (Natural, Map TokenName WholeNumber)
                -> (Natural, Map TokenName WholeNumber)
        unThese k = case k of
            This a    -> a
            That b    -> b
            These (ac, a) (bc, b) -> (ac + bc, fmap innerUnThese (M.union a b))

    in unThese <$> combined
-- For all currency symbols
-- the value must have the currency symbol
-- if it does then for all the tokens must be there with enough coins
-- remove those entries
satisfyExpectations :: ExpectedValue -> Value -> Bool
satisfyExpectations ev v = all (satisfyExpectation v) $ M.toList ev

satisfyExpectation
  :: Value
  -> (CurrencySymbol, (Natural, M.Map TokenName WholeNumber))
  -> Bool
satisfyExpectation theValue (cs, (Natural count, expectedTokenMap))
  = case M.lookup cs $ getValue theValue of
      Just actualTokenMap -> case validateTokenMap actualTokenMap expectedTokenMap of
         Just leftOverMap ->
           let validPolicyCount = sum (M.elems leftOverMap) >= count
           in TRACE_IF_FALSE("failed to validate policy count", "10", validPolicyCount)
         Nothing -> TRACE("failed to validate token map", "11", False)
      Nothing -> TRACE("failed to find policy", "12", False)

validateTokenMap :: M.Map TokenName Integer
                 -> M.Map TokenName WholeNumber
                 -> Maybe (M.Map TokenName Integer)
validateTokenMap actual expected
  = foldr (hasEnoughCoin actual) (Just actual)
  $ M.toList expected

hasEnoughCoin :: M.Map TokenName Integer
              -> (TokenName, WholeNumber)
              -> Maybe (M.Map TokenName Integer)
              -> Maybe (M.Map TokenName Integer)
hasEnoughCoin tokenMap (tkn, WholeNumber count) mAccumMap  = mAccumMap >>= \accumMap ->
  case M.lookup tkn tokenMap of
    Just actualCount
      | actualCount == count -> Just $ M.delete tkn accumMap
      | actualCount > count  -> Just $ M.insert tkn (actualCount - count) accumMap
      | otherwise            -> Nothing
    _ -> Nothing

valuePaidTo' :: [SwapTxOut] -> Address -> Value
valuePaidTo' outs addr = mconcat (addressOutputsAt addr outs)

addressOutputsAt :: Address -> [SwapTxOut] -> [Value]
addressOutputsAt addr outs =
  let
    flt SwapTxOut { sTxOutAddress, sTxOutValue }
      | addr == sTxOutAddress = Just sTxOutValue
      | otherwise = Nothing
  in mapMaybe flt outs

ownHash' :: [PartialSwapTxInInfo] -> TxOutRef -> ValidatorHash
ownHash' ins txOutRef = go ins where
    go = \case
      [] -> TRACE_ERROR("The impossible happened", "-1")
      PartialSwapTxInInfo {..} : xs ->
        if unsafeFromBuiltinData psTxInInfoOutRef == txOutRef then
          case psTxOutAddress psTxInInfoResolved of
            PartialSwapAddress (ScriptCredential s) _ -> s
            _ -> TRACE_ERROR("The impossible happened", "-1")
        else
          go xs

unstableMakeIsData ''SwapTxInfo
unstableMakeIsData ''SwapScriptContext
unstableMakeIsData ''PartialSwapAddress
unstableMakeIsData ''PartialSwapTxOut
unstableMakeIsData ''PartialSwapTxInInfo
unstableMakeIsData ''PartialSwapTxInfo
makeIsDataIndexed  ''SwapScriptPurpose [('ASpending,1)]
unstableMakeIsData ''SwapTxOut
unstableMakeIsData ''SwapTxInInfo

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data Payout = Payout
  { pAddress :: Address
  , pValue :: !ExpectedValue
  }

data Swap = Swap
  { sOwner :: PubKeyHash
  -- ^ Used for the signer check on Cancel
  , sSwapPayouts :: ![Payout]
  -- ^ Divvy up the payout to different address for Swap
  }

data Action
  = Cancel
  | Accept

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------
isScriptThisInput :: ValidatorHash -> PartialSwapTxInInfo -> Bool
isScriptThisInput vh PartialSwapTxInInfo { psTxInInfoResolved = PartialSwapTxOut { psTxOutAddress = PartialSwapAddress { psAddressCredential }}}
  = case psAddressCredential of
      ScriptCredential vh'
        | vh' == vh -> True
        | otherwise -> TRACE_ERROR("Wrong type of script input", "3")
      _ -> False

mapInsertWith :: Eq k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
mapInsertWith f k v xs = case M.lookup k xs of
  Nothing -> M.insert k v xs
  Just v' -> M.insert k (f v v') xs

mergePayouts :: Payout -> Map Address ExpectedValue -> Map Address ExpectedValue
mergePayouts Payout {..} =
  mapInsertWith unionExpectedValue pAddress pValue

paidAtleastTo :: [SwapTxOut] -> Address -> ExpectedValue -> Bool
paidAtleastTo outputs addr val = satisfyExpectations val (valuePaidTo' outputs addr)

hasConfigNft :: CurrencySymbol -> TokenName -> SwapTxInInfo -> Bool
hasConfigNft cs t SwapTxInInfo { sTxInInfoResolved = SwapTxOut { sTxOutValue = Value v }} = case M.lookup cs v of
  Nothing -> False
  Just m  -> case M.lookup t m of
    Nothing -> False
    Just c  -> c == 1

-------------------------------------------------------------------------------
-- Boilerplate
-------------------------------------------------------------------------------
instance Eq Payout where
  x == y = pAddress x == pAddress y && pValue x == pValue y

instance Eq Swap where
  x == y
    =  sOwner       x == sOwner       y
    && sSwapPayouts x == sSwapPayouts y

instance Eq Action where
  x == y = case (x, y) of
    (Cancel, Cancel) -> True
    (Cancel, _) -> False
    (Accept, Accept) -> True
    (Accept, _) -> False


data NonEmptyAddress = NonEmptyAddress
  { neaHead :: Address
  , neaTail :: [Address]
  }

{-# INLINABLE nonEmptyAddressToList #-}
nonEmptyAddressToList :: NonEmptyAddress -> [Address]
nonEmptyAddressToList (NonEmptyAddress x xs) = x : xs

data PaymentAddress = PaymentAddress
  { paPubKeyHash        :: PubKeyHash
  , paStakingCredential :: Maybe StakingCredential
  }

paymentAddressToAddress :: PaymentAddress -> Address
paymentAddressToAddress PaymentAddress {..} = Address
  { addressCredential        = PubKeyCredential paPubKeyHash
  , addressStakingCredential = paStakingCredential
  }

data NonEmptyPaymentAddress = NonEmptyPaymentAddress
  { nepaHead :: PaymentAddress
  , nepaTail :: [PaymentAddress]
  }

{-# INLINABLE nonEmptyPaymentAddressToList #-}
nonEmptyPaymentAddressToList :: NonEmptyPaymentAddress -> [PaymentAddress]
nonEmptyPaymentAddressToList (NonEmptyPaymentAddress x xs) = x : xs

data SwapDynamicConfig = SwapDynamicConfig
  { sdcValidOutputAddresses :: NonEmptyAddress
  -- ^ change this to addresses
  , sdcMarketplaceAddresses :: NonEmptyPaymentAddress
  }

unstableMakeIsData ''NonEmptyAddress
unstableMakeIsData ''PaymentAddress
unstableMakeIsData ''NonEmptyPaymentAddress
unstableMakeIsData ''SwapDynamicConfig
unstableMakeIsData ''Payout
unstableMakeIsData ''Swap
unstableMakeIsData ''Action

-------------------------------------------------------------------------------
-- Validation
-------------------------------------------------------------------------------
-- check that each user is paid
-- and the total is correct
{-# HLINT ignore validateOutputConstraints "Use uncurry" #-}
validateOutputConstraints :: [SwapTxOut] -> Map Address ExpectedValue -> Bool
validateOutputConstraints outputs constraints = all (\(addr, v) -> paidAtleastTo outputs addr v) (M.toList constraints)

-- Every branch but user initiated cancel requires checking the input
-- to ensure there is only one script input.
swapValidator :: SwapConfig -> BuiltinData -> Action -> SwapScriptContext -> Bool
swapValidator SwapConfig {..} _ r SwapScriptContext{sScriptContextTxInfo = partialInfo@PartialSwapTxInfo{..}, sScriptContextPurpose = ASpending thisOutRef} =
  let
    isSigner :: [PubKeyHash] -> PubKeyHash -> Bool
    isSigner signers keyHash = case signers of
      [] -> False
      x : xs' -> if x == keyHash then True else isSigner xs' keyHash

    thisValidator :: ValidatorHash
    !thisValidator = ownHash' pTxInfoInputs thisOutRef

    partialScriptInputs :: [PartialSwapTxInInfo]
    !partialScriptInputs = filter (isScriptThisInput thisValidator) pTxInfoInputs

  -- This allows the script to validate all inputs and outputs on only one script input.
  -- Ignores other script inputs being validated each time
  in if unsafeFromBuiltinData (psTxInInfoOutRef (head partialScriptInputs)) /= thisOutRef then True else
    let
      scriptInputs :: [SwapTxInInfo]
      !scriptInputs = map convertToSwapInputs partialScriptInputs

      !SwapTxInfo{..} = convertToSwapTxInfo partialInfo

      lookupSwapDynamicConfigDatum :: SwapTxInInfo -> SwapDynamicConfig
      lookupSwapDynamicConfigDatum SwapTxInInfo { sTxInInfoResolved = SwapTxOut {sTxOutDatum = theOutDatum}} =
        let Datum d = case theOutDatum of
              OutputDatum a -> a
              OutputDatumHash dh -> go (M.toList sTxInfoData) where
                go = \case
                  [] -> TRACE_ERROR("The impossible happened", "-1")
                  (k, v):xs' -> if k == dh then v else go xs'
              NoOutputDatum -> TRACE_ERROR("The impossible happened", "7")
        in FROM_BUILT_IN_DATA("lookupDatum datum conversion failed", "2", d, SwapDynamicConfig)

      -- find the configuration datum
      SwapDynamicConfig {..} = case filter (hasConfigNft scConfigNftPolicyId scConfigNftTokenName) sTxInfoReferenceInputs of
        [x] -> lookupSwapDynamicConfigDatum x
        _ -> traceError "missing or wrong number of swap config reference inputs!"

      lookupSwapDatum :: OutputDatum -> Swap
      lookupSwapDatum theOutDatum =
        let Datum d = case theOutDatum of
              OutputDatum a -> a
              OutputDatumHash dh -> go (M.toList sTxInfoData) where
                go = \case
                  [] -> TRACE_ERROR("The impossible happened", "-1")
                  (k, v):xs' -> if k == dh then v else go xs'
              NoOutputDatum -> TRACE_ERROR("The impossible happened", "7")
        in FROM_BUILT_IN_DATA("lookupDatum datum conversion failed", "2", d, Swap)

      swaps :: [(Swap, Value)]
      !swaps = case map (\SwapTxInInfo { sTxInInfoResolved = SwapTxOut {sTxOutDatum, sTxOutValue}} -> (lookupSwapDatum sTxOutDatum, sTxOutValue)) scriptInputs of
        [] -> TRACE_ERROR("The impossible happened", "6")
        xs -> xs

      outputsAreValid :: Map Address ExpectedValue -> Bool
      outputsAreValid = validateOutputConstraints sTxInfoOutputs

    in case r of
      Cancel ->
        let
          signerIsOwner :: Swap -> Bool
          signerIsOwner Swap{sOwner} = isSigner sTxInfoSignatories sOwner

          allScriptValue :: Value
          !allScriptValue =
            foldr (\SwapTxInInfo {sTxInInfoResolved = SwapTxOut {sTxOutValue}} acc ->
                    sTxOutValue <> acc
                  )
                  mempty
                  scriptInputs

          isValidOutput :: Address -> Bool
          isValidOutput address@Address {addressCredential}
            =  any (address==) (nonEmptyAddressToList sdcValidOutputAddresses)
            || any (\(Swap{sOwner}, _) -> addressCredential == PubKeyCredential sOwner) swaps

          validOutputsValue :: Value
          !validOutputsValue =
            foldr (\SwapTxOut {sTxOutValue, sTxOutAddress} acc ->
                    if isValidOutput sTxOutAddress then sTxOutValue <> acc else acc
                  )
                  mempty
                  sTxInfoOutputs

          outputAddressesAreValid :: Bool
          !outputAddressesAreValid = validOutputsValue `geq` allScriptValue

        in traceIfFalse "signer is not the owner" (all (signerIsOwner . fst) swaps)
        && traceIfFalse "wrong output address" outputAddressesAreValid

      Accept ->
        -- Acts like a buy, but we ignore any payouts that go to the signer of the
        -- transaction. This allows the seller to accept an offer from a buyer that
        -- does not pay the seller as much as they requested
        -- Determining the total sale ada is tricky, because users could cancel their
        -- listing while accepting another users offer.
        -- To handle these cases, we sum the payout ada and the total script input
        -- ada of all the inputs the signing user does not own, and then take the
        -- max of the two.
        let
          accumPayouts :: (Swap, Value) -> (Map Address ExpectedValue, Integer, Integer) -> (Map Address ExpectedValue, Integer, Integer)
          accumPayouts (Swap{..}, v) acc@(payoutMap, theTotalPayoutAda, theTotalScriptAda)
            | isSigner sTxInfoSignatories sOwner = acc
            | otherwise =
                ( foldr mergePayouts payoutMap sSwapPayouts
                , foldr (\Payout {pValue} theAda -> theAda + expectedLovelaces pValue) theTotalPayoutAda sSwapPayouts
                , theTotalScriptAda + lovelaces v
                )

          -- assume all redeemers are accept, all the payouts should be paid (excpet those to the signer)
          payouts :: Map Address ExpectedValue
          totalPayoutAda :: Integer
          totalScriptAda :: Integer

          (!payouts, !totalPayoutAda, !totalScriptAda) =
            foldr accumPayouts (M.empty, 0, 0) swaps

          saleAda :: Integer
          !saleAda = max totalPayoutAda totalScriptAda

          expectedMarketplaceValue :: ExpectedValue
          !expectedMarketplaceValue =
            M.singleton
              adaSymbol
              ( Natural 0
              , M.singleton adaToken (WholeNumber (max 1 ((saleAda * scMarketplaceFee) `divide` 1000)))
              )

          -- We assume there are not other marketplace payouts
          finalPayouts :: Map Address ExpectedValue
          !finalPayouts = M.insert (paymentAddressToAddress (nepaHead sdcMarketplaceAddresses)) expectedMarketplaceValue payouts

          marketPlaceSigned :: Bool
          !marketPlaceSigned =
            any (\x -> any (\addr -> x == paPubKeyHash addr) (nonEmptyPaymentAddressToList sdcMarketplaceAddresses)) sTxInfoSignatories

        in traceIfFalse "wrong output" (outputsAreValid finalPayouts)
        && traceIfFalse "missing the marketplace signature" marketPlaceSigned
-------------------------------------------------------------------------------
-- Entry Points
-------------------------------------------------------------------------------

swapWrapped :: SwapConfig -> BuiltinData -> BuiltinData -> BuiltinData -> ()
swapWrapped = wrap . swapValidator

validator :: SwapConfig -> Validator
validator cfg =
  Plutonomy.optimizeUPLC $ Plutonomy.validatorToPlutus $ Plutonomy.mkValidatorScript $
    $$(PlutusTx.compile [|| swapWrapped ||])
    `applyCode`
    liftCode cfg

swap :: SwapConfig -> PlutusScript PlutusScriptV2
swap = PlutusScriptSerialised . SBS.toShort . LB.toStrict . serialise . validator

swapHash :: SwapConfig -> ValidatorHash
swapHash = validatorHash . validator

writePlutusFile :: SwapConfig -> FilePath -> IO ()
writePlutusFile cfg filePath = Api.writeFileTextEnvelope filePath Nothing (swap cfg) >>= \case
  Left err -> print $ Api.displayError err
  Right () -> putStrLn $ "wrote NFT validator to file " ++ filePath
