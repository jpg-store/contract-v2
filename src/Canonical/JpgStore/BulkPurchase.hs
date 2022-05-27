{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Canonical.JpgStore.BulkPurchase
  ( ExpectedValue
  , Payout(..)
  , Redeemer(..)
  , Swap(..)
  , swap
  , writePlutusFile
  , unionExpectedValue
  ) where

{- HLINT ignore module "Eta reduce" -}
{- HLINT ignore module "Avoid lambda" -}

import Canonical.Shared
import qualified Cardano.Api as Api
import Cardano.Api.Shelley (PlutusScript(..), PlutusScriptV1)
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as SBS
import Ledger
  (Datum(..), DatumHash, PubKeyHash, TxOutRef, mkValidatorScript, ValidatorHash, validatorHash)
import qualified Ledger.Typed.Scripts as Scripts
import Plutus.V1.Ledger.Credential
import Plutus.V1.Ledger.Value
import PlutusTx
import qualified PlutusTx.AssocMap as M
import PlutusTx.AssocMap (Map)
import PlutusTx.Prelude
import Prelude (IO, print, putStrLn)
import Plutus.V1.Ledger.Ada
import System.FilePath
import PlutusTx.These

#include "../DebugUtilities.h"

newtype WholeNumber = WholeNumber { unWholeNumber :: Integer }
  deriving(Eq, AdditiveSemigroup, Ord, ToData)

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
  , sAddressStakingCredential :: BuiltinData
  }

data SwapTxOut = SwapTxOut
  { sTxOutAddress :: SwapAddress
  , sTxOutValue :: Value
  , sTxOutDatumHash :: Maybe DatumHash
  }

data SwapTxInInfo = SwapTxInInfo
  { sTxInInfoOutRef :: TxOutRef
  , sTxInInfoResolved :: SwapTxOut
  }

data SwapTxInfo = SwapTxInfo
  { sTxInfoInputs :: [SwapTxInInfo]
  , sTxInfoOutputs :: [SwapTxOut]
  , sTxInfoFee :: BuiltinData
  , sTxInfoMint :: BuiltinData
  , sTxInfoDCert :: BuiltinData
  , sTxInfoWdrl :: BuiltinData
  , sTxInfoValidRange :: BuiltinData
  , sTxInfoSignatories :: [PubKeyHash]
  , sTxInfoData :: [(DatumHash, Datum)]
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

expectedLovelaces :: ExpectedValue -> Integer
expectedLovelaces e = maybe 0 unWholeNumber $  do
  tm <- snd <$> M.lookup adaSymbol e
  M.lookup adaToken tm

{-
subtractLovelaces :: ExpectedValue -> Integer -> ExpectedValue
subtractLovelaces ev loves = fromMaybe ev $ do
  (c, tm) <- M.lookup adaSymbol ev
  oldLovelaces <- M.lookup adaToken tm
  let newTm = M.insert adaToken (oldLovelaces - loves) tm
  pure $ M.insert adaSymbol (c, newTm) ev
-}
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
           let validPolicyCount = length (M.keys leftOverMap) >= count
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
      | actualCount >= count -> Just $ M.delete tkn accumMap
    _ -> Nothing

valuePaidTo' :: [SwapTxOut] -> PubKeyHash -> Value
valuePaidTo' outs pkh = mconcat (pubKeyOutputsAt' pkh outs)

pubKeyOutputsAt' :: PubKeyHash -> [SwapTxOut] -> [Value]
pubKeyOutputsAt' pk outs =
  let
    flt SwapTxOut { sTxOutAddress = SwapAddress (PubKeyCredential pk') _, sTxOutValue }
      | pk == pk' = Just sTxOutValue
      | otherwise = Nothing
    flt _ = Nothing
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
  { pAddress :: !PubKeyHash
  , pValue :: !ExpectedValue
  }

data Swap = Swap
  { sOwner :: !PubKeyHash
  -- ^ Used for the signer check on Cancel
  , sSwapPayouts :: ![Payout]
  -- ^ Divvy up the payout to different address for Swap
  }

data Redeemer
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

mergePayouts :: Payout -> Map PubKeyHash ExpectedValue -> Map PubKeyHash ExpectedValue
mergePayouts Payout {..} =
  mapInsertWith unionExpectedValue pAddress pValue

paidAtleastTo :: [SwapTxOut] -> PubKeyHash -> ExpectedValue -> Bool
paidAtleastTo outputs pkh val = satisfyExpectations val (valuePaidTo' outputs pkh)
-------------------------------------------------------------------------------
-- Boilerplate
-------------------------------------------------------------------------------
instance Eq Payout where
  x == y = pAddress x == pAddress y && pValue x == pValue y

instance Eq Swap where
  x == y
    =  sOwner       x == sOwner       y
    && sSwapPayouts x == sSwapPayouts y

instance Eq Redeemer where
  x == y = case (x, y) of
    (Cancel, Cancel) -> True
    (Cancel, _) -> False
    (Accept, Accept) -> True
    (Accept, _) -> False

PlutusTx.unstableMakeIsData ''Payout
PlutusTx.unstableMakeIsData ''Swap
PlutusTx.unstableMakeIsData ''Redeemer

-------------------------------------------------------------------------------
-- Validation
-------------------------------------------------------------------------------
-- check that each user is paid
-- and the total is correct
{-# HLINT ignore validateOutputConstraints "Use uncurry" #-}
validateOutputConstraints :: [SwapTxOut] -> Map PubKeyHash ExpectedValue -> Bool
validateOutputConstraints outputs constraints = all (\(pkh, v) -> paidAtleastTo outputs pkh v) (M.toList constraints)

-- Every branch but user initiated cancel requires checking the input
-- to ensure there is only one script input.
swapValidator :: BuiltinData -> Redeemer -> SwapScriptContext -> Bool
swapValidator _ r SwapScriptContext{sScriptContextTxInfo = SwapTxInfo{..}, sScriptContextPurpose = ASpending thisOutRef} =
  let
    singleSigner :: PubKeyHash
    singleSigner = case sTxInfoSignatories of
      [x] -> x
      _ -> TRACE_ERROR("single signer expected", "1")

    thisValidator :: ValidatorHash
    thisValidator = ownHash' sTxInfoInputs thisOutRef

    convertDatum :: Datum -> Swap
    convertDatum d =
      let theSwap = getDatum d
      in FROM_BUILT_IN_DATA("found datum that is not a swap", "2", theSwap, Swap)

    lookupDatum :: SwapTxInInfo -> Datum
    lookupDatum SwapTxInInfo { sTxInInfoResolved = SwapTxOut {sTxOutDatumHash = Just dh}} = go sTxInfoData where
      go = \case
        [] -> TRACE_ERROR("The impossible happened", "-1")
        (k, v):xs' -> if k == dh then v else go xs'
    lookupDatum _ = TRACE_ERROR("The impossible happened", "7")

    scriptInputs :: [SwapTxInInfo]
    scriptInputs = filter (isScriptThisInput thisValidator) sTxInfoInputs

    -- Defensive mapMaybe and fail
    -- Die if Swaps is empty
    swaps :: [Swap]
    swaps = case map (\i -> convertDatum (lookupDatum i)) scriptInputs of
      [] -> TRACE_ERROR("The impossible happened", "6")
      xs -> xs

    outputsAreValid :: Map PubKeyHash ExpectedValue -> Bool
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
          accumPayouts :: Swap -> Map PubKeyHash ExpectedValue -> Map PubKeyHash ExpectedValue
          accumPayouts Swap{..} acc
            | sOwner == singleSigner = acc
            | otherwise = foldr mergePayouts acc sSwapPayouts

          -- assume all redeemers are accept, all the payouts should be paid (excpet those to the signer)
          payouts :: Map PubKeyHash ExpectedValue
          payouts = foldr accumPayouts M.empty swaps
        in TRACE_IF_FALSE("wrong output", "5", (outputsAreValid payouts))
-------------------------------------------------------------------------------
-- Entry Points
-------------------------------------------------------------------------------

swapWrapped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
swapWrapped = wrap swapValidator

validator :: Scripts.Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| swapWrapped ||])

swap :: PlutusScript PlutusScriptV1
swap = PlutusScriptSerialised . SBS.toShort . LB.toStrict . serialise $ validator

swapHash :: ValidatorHash
swapHash = validatorHash validator

writePlutusFile :: FilePath -> IO ()
writePlutusFile filePath = Api.writeFileTextEnvelope filePath Nothing swap >>= \case
  Left err -> print $ Api.displayError err
  Right () -> putStrLn $ "wrote NFT validator to file " ++ filePath
