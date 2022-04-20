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
  (Datum(..), DatumHash, POSIXTime, POSIXTimeRange, PubKeyHash, TxOutRef, after, before, mkValidatorScript)
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
  { aaddressCredential :: Credential
  , aaddressStakingCredential :: BuiltinData
  }

data SwapTxOut = SwapTxOut
  { atxOutAddress :: SwapAddress
  , atxOutValue :: Value
  , atxOutDatumHash :: BuiltinData
  }

data SwapTxInInfo = SwapTxInInfo
  { atxInInfoOutRef :: TxOutRef
  , atxInInfoResolved :: SwapTxOut
  }

data SwapTxInfo = SwapTxInfo
  { atxInfoInputs :: [SwapTxInInfo]
  , atxInfoOutputs :: [SwapTxOut]
  , atxInfoFee :: BuiltinData
  , atxInfoMint :: BuiltinData
  , atxInfoDCert :: BuiltinData
  , atxInfoWdrl :: BuiltinData
  , atxInfoValidRange :: POSIXTimeRange
  , atxInfoSignatories :: [PubKeyHash]
  , atxInfoData :: [(DatumHash, Datum)]
  , atxInfoId :: BuiltinData
  }

data SwapScriptPurpose
    = ASpending TxOutRef

data SwapScriptContext = SwapScriptContext
  { aScriptContextTxInfo :: SwapTxInfo
  , aScriptContextPurpose :: SwapScriptPurpose
  }

valuePaidTo' :: [SwapTxOut] -> PubKeyHash -> Value
valuePaidTo' outs pkh = mconcat (pubKeyOutputsAt' pkh outs)

pubKeyOutputsAt' :: PubKeyHash -> [SwapTxOut] -> [Value]
pubKeyOutputsAt' pk outs =
  let
    flt SwapTxOut { atxOutAddress = SwapAddress (PubKeyCredential pk') _, atxOutValue }
      | pk == pk' = Just atxOutValue
      | otherwise = Nothing
    flt _ = Nothing
  in mapMaybe flt outs

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
  , sDeadline :: !(Maybe POSIXTime)
  -- ^ Buys are only accepted until this deadline. After the deadline passes anyone can use a close redeemer to
  -- send the assets back to the owner.
  }

data Redeemer
  = Buy
  | Cancel
  | Close

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------
isScriptAddress :: SwapAddress -> Bool
isScriptAddress SwapAddress { aaddressCredential } = case aaddressCredential of
  ScriptCredential _ -> True
  _ -> False

isScriptInput :: SwapTxInInfo -> Bool
isScriptInput txIn = isScriptAddress (atxOutAddress  (atxInInfoResolved txIn))

mapInsertWith :: Eq k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
mapInsertWith f k v xs = case M.lookup k xs of
  Nothing -> M.insert k v xs
  Just v' -> M.insert k (f v v') xs

mergePayouts :: Payout -> Map PubKeyHash Value -> Map PubKeyHash Value
mergePayouts Payout {..} =
  mapInsertWith (+) pAddress pValue

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
      && sDeadline x
      == sDeadline y

instance Eq Redeemer where
  x == y = case (x, y) of
    (Buy, Buy) -> True
    (Cancel, Cancel) -> True
    (Close, Close) -> True
    _ -> False

PlutusTx.unstableMakeIsData ''Payout
PlutusTx.unstableMakeIsData ''Swap
PlutusTx.unstableMakeIsData ''Redeemer

-------------------------------------------------------------------------------
-- Validation
-------------------------------------------------------------------------------
-- check that each user is paid
-- and the total is correct
validateOutputConstraints :: [SwapTxOut] -> Map PubKeyHash Value -> Bool
validateOutputConstraints outputs constraints = all (\(pkh, v) -> paidAtleastTo outputs pkh v) (M.toList constraints)

-- Every branch but user initiated cancel requires checking the input
-- to ensure there is only one script input.
swapValidator :: Swap -> Redeemer -> SwapScriptContext -> Bool
swapValidator _ r SwapScriptContext{aScriptContextTxInfo = SwapTxInfo{..}, aScriptContextPurpose = ASpending thisOutRef} =
  let
    singleSigner :: PubKeyHash
    singleSigner = case atxInfoSignatories of
      [x] -> x
      _ -> TRACE_ERROR("single signer expected", "1")

    -- Verify that the script inputs are all for this script
    validScriptInputs :: Bool
    validScriptInputs =
      any (\x -> not (isScriptInput x)) atxInfoInputs

    convertDatum :: forall a. DataConstraint(a) => Datum -> a
    convertDatum d =
      let a = getDatum d
       in FROM_BUILT_IN_DATA("found datum that is not a swap", "2", a)

    swaps :: [Swap]
    swaps = fmap (\(_, d) -> convertDatum d) atxInfoData

    outputsAreValid :: Map PubKeyHash Value -> Bool
    outputsAreValid = validateOutputConstraints atxInfoOutputs

    foldSwaps :: (Swap -> a -> a) -> a -> a
    foldSwaps f init = foldr f init swaps

  in if atxInInfoOutRef (head (filter isScriptInput atxInfoInputs)) /= thisOutRef then True else
    TRACE_IF_FALSE("invalid script inputs", "3", validScriptInputs) && case r of
      Cancel ->
        let
          signerIsOwner Swap{sOwner} = singleSigner == sOwner
        in
          TRACE_IF_FALSE("signer is not the owner", "4", (all signerIsOwner swaps))
      Close ->
        -- assume all redeemers are Close and all the assets are going back to their owner
        let
          f Swap{..} payouts =
            case sDeadline of
              Nothing -> TRACE_ERROR("swap has no deadline", "5")
              Just deadline | deadline `before` atxInfoValidRange -> mergePayouts (Payout sOwner sSwapValue) payouts
              Just _ -> TRACE_ERROR("deadline not passed", "6")

          assets :: Map PubKeyHash Value
          assets = foldSwaps f mempty
        in
          TRACE_IF_FALSE("wrong output", "7", (outputsAreValid assets))
      Buy ->
        let
          accumPayouts Swap{sSwapValue, sSwapPayouts, sDeadline} acc =
            if all (`after` atxInfoValidRange) sDeadline
              then foldr mergePayouts acc (Payout singleSigner sSwapValue : sSwapPayouts)
              else TRACE_ERROR("deadline is passed", "8")

          -- assume all redeemers are buys, all the payouts should be paid, and the assets should go to the tx signer (aka the buyer)
          payouts :: Map PubKeyHash Value
          payouts = foldSwaps accumPayouts mempty
        in TRACE_IF_FALSE("wrong output", "9", (outputsAreValid payouts))

-------------------------------------------------------------------------------
-- Entry Points
-------------------------------------------------------------------------------

swapWrapped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
swapWrapped = wrap swapValidator

validator :: Scripts.Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| swapWrapped ||])

swap :: PlutusScript PlutusScriptV1
swap = PlutusScriptSerialised . SBS.toShort . LB.toStrict . serialise $ validator

writePlutusFile :: FilePath -> IO ()
writePlutusFile filePath = Api.writeFileTextEnvelope filePath Nothing swap >>= \case
  Left err -> print $ Api.displayError err
  Right () -> putStrLn $ "wrote NFT validator to file " ++ filePath
