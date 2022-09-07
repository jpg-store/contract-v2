{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Canonical.JpgStore.BulkPurchase
  ( ExpectedValue
  , Natural(..)
  , Payout(..)
  , Action(..)
  , Swap(..)
  , SwapAddress(..)
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
import Cardano.Api.Shelley (PlutusScript(..), PlutusScriptV2)
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as SBS
import           Plutus.V1.Ledger.Crypto
import           Plutus.V2.Ledger.Contexts
import           Plutus.V1.Ledger.Scripts
import           Plutus.V2.Ledger.Tx
import Plutus.V1.Ledger.Credential
import Plutus.V1.Ledger.Value
import PlutusTx
import qualified PlutusTx.AssocMap as M
import PlutusTx.AssocMap (Map)
import PlutusTx.Prelude
import Prelude (IO, print, putStrLn)
import System.FilePath
import PlutusTx.These
import qualified Plutonomy

#include "../DebugUtilities.h"

newtype WholeNumber = WholeNumber { unWholeNumber :: Integer }
  deriving(Eq, AdditiveSemigroup, Ord, ToData)

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
  deriving(Eq, AdditiveSemigroup, AdditiveMonoid, Ord, ToData)

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

data SwapAddress = SwapAddress
  { sAddressCredential :: Credential
  , sAddressStakingCredential :: Maybe StakingCredential
  }

instance Eq SwapAddress where
  x == y
    =  sAddressCredential        x == sAddressCredential        y
    && sAddressStakingCredential x == sAddressStakingCredential y

data SwapTxOut = SwapTxOut
  { sTxOutAddress :: SwapAddress
  , sTxOutValue :: Value
  , sTxOutDatum           :: OutputDatum
  , sTxOutReferenceScript :: BuiltinData
  }

data SwapTxInInfo = SwapTxInInfo
  { sTxInInfoOutRef :: TxOutRef
  , sTxInInfoResolved :: SwapTxOut
  }

data SwapTxInfo = SwapTxInfo
  { sTxInfoInputs :: [SwapTxInInfo]
  , sTxInfoReferenceInputs    :: BuiltinData
  , sTxInfoOutputs :: [SwapTxOut]
  , sTxInfoFee :: BuiltinData
  , sTxInfoMint :: BuiltinData
  , sTxInfoDCert :: BuiltinData
  , sTxInfoWdrl :: BuiltinData
  , sTxInfoValidRange :: BuiltinData
  , sTxInfoSignatories :: [PubKeyHash]
  , sTxInfoRedeemers          :: BuiltinData
  , sTxInfoData :: Map DatumHash Datum
  , sTxInfoId :: BuiltinData
  }

{- HLINT ignore SwapScriptPurpose -}
data SwapScriptPurpose
    = ASpending TxOutRef

data SwapScriptContext = SwapScriptContext
  { sScriptContextTxInfo :: SwapTxInfo
  , sScriptContextPurpose :: SwapScriptPurpose
  }

type ExpectedValue = M.Map CurrencySymbol (Natural, M.Map TokenName WholeNumber)

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

valuePaidTo' :: [SwapTxOut] -> SwapAddress -> Value
valuePaidTo' outs addr = mconcat (addressOutputsAt addr outs)

addressOutputsAt :: SwapAddress -> [SwapTxOut] -> [Value]
addressOutputsAt addr outs =
  let
    flt SwapTxOut { sTxOutAddress, sTxOutValue }
      | addr == sTxOutAddress = Just sTxOutValue
      | otherwise = Nothing
  in mapMaybe flt outs

ownHash' :: [SwapTxInInfo] -> TxOutRef -> ValidatorHash
ownHash' ins txOutRef = go ins where
    go = \case
      [] -> TRACE_ERROR("The impossible happened", "-1")
      SwapTxInInfo {..} :xs ->
        if sTxInInfoOutRef == txOutRef then
          case sTxOutAddress sTxInInfoResolved of
            SwapAddress (ScriptCredential s) _ -> s
            _ -> TRACE_ERROR("The impossible happened", "-1")
        else
          go xs

unstableMakeIsData ''SwapTxInfo
unstableMakeIsData ''SwapScriptContext
makeIsDataIndexed  ''SwapScriptPurpose [('ASpending,1)]
unstableMakeIsData ''SwapAddress
unstableMakeIsData ''SwapTxOut
unstableMakeIsData ''SwapTxInInfo

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data Payout = Payout
  { pAddress :: SwapAddress
  , pValue :: ExpectedValue
  }

data Swap = Swap
  { sOwner :: PubKeyHash
  -- ^ Used for the signer check on Cancel
  , sSwapPayouts :: [Payout]
  -- ^ Divvy up the payout to different address for Swap
  }

data Action
  = Cancel
  | Accept

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------
isScriptThisInput :: ValidatorHash -> SwapTxInInfo -> Bool
isScriptThisInput vh txIn = case sAddressCredential (sTxOutAddress  (sTxInInfoResolved txIn)) of
  ScriptCredential vh'
    | vh' == vh -> True
    | otherwise -> TRACE_ERROR("Wrong type of script input", "3")
  _ -> False

mapInsertWith :: Eq k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
mapInsertWith f k v xs = case M.lookup k xs of
  Nothing -> M.insert k v xs
  Just v' -> M.insert k (f v v') xs

mergePayouts :: Payout -> Map SwapAddress ExpectedValue -> Map SwapAddress ExpectedValue
mergePayouts Payout {..} =
  mapInsertWith unionExpectedValue pAddress pValue

paidAtleastTo :: [SwapTxOut] -> SwapAddress -> ExpectedValue -> Bool
paidAtleastTo outputs addr val = satisfyExpectations val (valuePaidTo' outputs addr)

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

PlutusTx.unstableMakeIsData ''Payout
PlutusTx.unstableMakeIsData ''Swap
PlutusTx.unstableMakeIsData ''Action

-------------------------------------------------------------------------------
-- Validation
-------------------------------------------------------------------------------
-- check that each user is paid
-- and the total is correct
{-# HLINT ignore validateOutputConstraints "Use uncurry" #-}
validateOutputConstraints :: [SwapTxOut] -> Map SwapAddress ExpectedValue -> Bool
validateOutputConstraints outputs constraints = all (\(addr, v) -> paidAtleastTo outputs addr v) (M.toList constraints)

-- Every branch but user initiated cancel requires checking the input
-- to ensure there is only one script input.
swapValidator :: BuiltinData -> Action -> SwapScriptContext -> Bool
swapValidator _ r SwapScriptContext{sScriptContextTxInfo = SwapTxInfo{..}, sScriptContextPurpose = ASpending thisOutRef} =
  let
    singleSigner :: PubKeyHash
    singleSigner = case sTxInfoSignatories of
      [x] -> x
      _ -> TRACE_ERROR("single signer expected", "1")

    thisValidator :: ValidatorHash
    thisValidator = ownHash' sTxInfoInputs thisOutRef

    lookupDatum :: SwapTxInInfo -> Swap
    lookupDatum SwapTxInInfo { sTxInInfoResolved = SwapTxOut {sTxOutDatum = theOutDatum}} =
      let Datum d = case theOutDatum of
            OutputDatum a -> a
            OutputDatumHash dh -> go (M.toList sTxInfoData) where
              go = \case
                [] -> TRACE_ERROR("The impossible happened", "-1")
                (k, v):xs' -> if k == dh then v else go xs'
            NoOutputDatum -> TRACE_ERROR("The impossible happened", "7")
      in FROM_BUILT_IN_DATA("lookupDatum datum conversion failed", "2", d, Swap)

    scriptInputs :: [SwapTxInInfo]
    scriptInputs = filter (isScriptThisInput thisValidator) sTxInfoInputs

    swaps :: [Swap]
    swaps = case map (\i -> lookupDatum i) scriptInputs of
      [] -> TRACE_ERROR("The impossible happened", "6")
      xs -> xs

    outputsAreValid :: Map SwapAddress ExpectedValue -> Bool
    outputsAreValid = validateOutputConstraints sTxInfoOutputs

  -- This allows the script to validate all inputs and outputs on only one script input.
  -- Ignores other script inputs being validated each time
  in if sTxInInfoOutRef (head scriptInputs) /= thisOutRef then True else
    case r of
      Cancel ->
        let
          signerIsOwner :: Swap -> Bool
          signerIsOwner Swap{sOwner} = singleSigner == sOwner
        in TRACE_IF_FALSE("signer is not the owner", "4", (all signerIsOwner swaps))

      Accept ->
        -- Acts like a buy, but we ignore any payouts that go to the signer of the
        -- transaction. This allows the seller to accept an offer from a buyer that
        -- does not pay the seller as much as they requested
        let
          accumPayouts :: Swap -> Map SwapAddress ExpectedValue -> Map SwapAddress ExpectedValue
          accumPayouts Swap{..} acc
            | sOwner == singleSigner = acc
            | otherwise = foldr mergePayouts acc sSwapPayouts

          -- assume all redeemers are accept, all the payouts should be paid (excpet those to the signer)
          payouts :: Map SwapAddress ExpectedValue
          payouts = foldr accumPayouts M.empty swaps
        in TRACE_IF_FALSE("wrong output", "5", (outputsAreValid payouts))
-------------------------------------------------------------------------------
-- Entry Points
-------------------------------------------------------------------------------

swapWrapped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
swapWrapped = wrap swapValidator

validator :: Validator
validator = Plutonomy.optimizeUPLC $ Plutonomy.validatorToPlutus $ Plutonomy.mkValidatorScript $$(PlutusTx.compile [|| swapWrapped ||])

swap :: PlutusScript PlutusScriptV2
swap = PlutusScriptSerialised . SBS.toShort . LB.toStrict . serialise $ validator

swapHash :: ValidatorHash
swapHash = validatorHash validator

writePlutusFile :: FilePath -> IO ()
writePlutusFile filePath = Api.writeFileTextEnvelope filePath Nothing swap >>= \case
  Left err -> print $ Api.displayError err
  Right () -> putStrLn $ "wrote NFT validator to file " ++ filePath
