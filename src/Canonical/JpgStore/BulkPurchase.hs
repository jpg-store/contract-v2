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
  ( Payout(..)
  , Redeemer(..)
  , Swap(..)
  , swap
  , writePlutusFile
  ) where

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
import System.FilePath

#include "../DebugUtilities.h"

data SwapAddress = SwapAddress
  { sAddressCredential :: Credential
  , sAddressStakingCredential :: BuiltinData
  }

data SwapTxOut = SwapTxOut
  { sTxOutAddress :: SwapAddress
  , sTxOutValue :: Value
  , sTxOutDatumHash :: BuiltinData
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

{-# HLINT ignore SwapScriptPurpose #-}
data SwapScriptPurpose
    = ASpending TxOutRef

data SwapScriptContext = SwapScriptContext
  { sScriptContextTxInfo :: SwapTxInfo
  , sScriptContextPurpose :: SwapScriptPurpose
  }

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
  , pValue :: !Value
  }

data Swap = Swap
  { sOwner :: !PubKeyHash
  -- ^ Used for the signer check on Cancel
  , sSwapValue :: !Value
  -- ^ Value the owner is offering up
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

onlyThisTypeOfScript :: ValidatorHash -> [SwapTxInInfo] -> Bool
onlyThisTypeOfScript thisValidator = go where
  go = \case
    [] -> True
    SwapTxInInfo
      { sTxInInfoResolved = SwapTxOut
        { sTxOutAddress = SwapAddress
          { sAddressCredential = ScriptCredential vh
          }
        }
      } : xs ->  if vh == thisValidator then
              go xs
            else
              TRACE_IF_FALSE("Bad validator input", "100", False)
    _ : xs -> go xs

mapInsertWith :: Eq k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
mapInsertWith f k v xs = case M.lookup k xs of
  Nothing -> M.insert k v xs
  Just v' -> M.insert k (f v v') xs

absoluteValueAdd :: Value -> Value -> Value
absoluteValueAdd x y = unionWith (\x' y' -> abs x' + abs y') x y

mergePayouts :: Payout -> Map PubKeyHash Value -> Map PubKeyHash Value
mergePayouts Payout {..} =
  mapInsertWith absoluteValueAdd pAddress pValue

paidAtleastTo :: [SwapTxOut] -> PubKeyHash -> Value -> Bool
paidAtleastTo outputs pkh val = valuePaidTo' outputs pkh `geq` val
-------------------------------------------------------------------------------
-- Boilerplate
-------------------------------------------------------------------------------
instance Eq Payout where
  x == y = pAddress x == pAddress y && pValue x == pValue y

instance Eq Swap where
  x == y =
    sOwner x
      == sOwner y
      && sSwapValue x
      == sSwapValue y
      && sSwapPayouts x
      == sSwapPayouts y

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
validateOutputConstraints :: [SwapTxOut] -> Map PubKeyHash Value -> Bool
validateOutputConstraints outputs constraints = all (\(pkh, v) -> paidAtleastTo outputs pkh v) (M.toList constraints)

-- Every branch but user initiated cancel requires checking the input
-- to ensure there is only one script input.
swapValidator :: Swap -> Redeemer -> SwapScriptContext -> Bool
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

    swaps :: [Swap]
    swaps = map (\(_, d) -> convertDatum d) sTxInfoData

    outputsAreValid :: Map PubKeyHash Value -> Bool
    outputsAreValid = validateOutputConstraints sTxInfoOutputs

    foldSwaps :: (Swap -> Map PubKeyHash Value -> Map PubKeyHash Value) -> Map PubKeyHash Value -> Map PubKeyHash Value
    foldSwaps f init = foldr f init swaps
  -- This allows the script to validate all inputs and outputs on only one script input.
  -- Ignores other script inputs being validated each time
  in if sTxInInfoOutRef (head (filter (isScriptThisInput thisValidator) sTxInfoInputs)) /= thisOutRef then True else
    case r of
      Cancel ->
        let
          signerIsOwner Swap{sOwner} = singleSigner == sOwner
        in
          TRACE_IF_FALSE("signer is not the owner", "4", (all signerIsOwner swaps))

      Accept ->
        -- Acts like a buy, but we ignore any payouts that go to the signer of the
        -- transaction. This allows the seller to accept an offer from a buyer that
        -- does not pay the seller as much as they requested
        let
          accumPayouts Swap{..} acc
            | sOwner == singleSigner = acc
            | otherwise = foldr mergePayouts acc sSwapPayouts

          -- assume all redeemers are accept, all the payouts should be paid (excpet those to the signer)
          payouts :: Map PubKeyHash Value
          payouts = foldSwaps accumPayouts mempty
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
