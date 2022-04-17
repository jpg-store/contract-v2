{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
  (Datum(..), DatumHash, POSIXTime, POSIXTimeRange, PubKeyHash, after, before, mkValidatorScript)
import qualified Ledger.Typed.Scripts as Scripts
import Plutus.V1.Ledger.Contexts
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
  , atxOutDatumHash :: Maybe DatumHash
  }

data SwapTxInInfo = SwapTxInInfo
  { atxInInfoOutRef :: TxOutRef
  , atxInInfoResolved :: SwapTxOut
  }

data SwapTxInfo = SwapTxInfo
  { atxInfoInputs :: [SwapTxInInfo]
  , atxInfoOutputs :: [SwapTxOut]
  , atxInfoFee :: BuiltinData
  , atxInfoMint :: Value
  , atxInfoDCert :: BuiltinData
  , atxInfoWdrl :: BuiltinData
  , atxInfoValidRange :: POSIXTimeRange
  , atxInfoSignatories :: [PubKeyHash]
  , atxInfoData :: [(DatumHash, Datum)]
  , atxInfoId :: BuiltinData
  }

data SwapScriptPurpose = ASpending TxOutRef

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

makeIsDataIndexed  ''SwapScriptPurpose [('ASpending,1)]
unstableMakeIsData ''SwapTxInfo
unstableMakeIsData ''SwapScriptContext
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

payoutToInequality :: Payout -> (PubKeyHash, Value)
payoutToInequality Payout {..} = (pAddress, pValue)

mergePayoutsValue :: [Payout] -> Value
mergePayoutsValue = foldr (\x acc -> pValue x <> acc) mempty

paidAtleastTo :: SwapTxInfo -> PubKeyHash -> Value -> Bool
paidAtleastTo info pkh val = valuePaidTo' (atxInfoOutputs info) pkh `geq` val

mergeInequalities :: Map PubKeyHash Value -> Map PubKeyHash Value -> Map PubKeyHash Value
mergeInequalities = M.unionWith (+)

mergeAll :: [Map PubKeyHash Value] -> Map PubKeyHash Value
mergeAll = foldr mergeInequalities M.empty

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
    (Buy, _) -> False
    (Cancel, Cancel) -> True
    (Cancel, _) -> False
    (Close, Close) -> True
    (Close, _) -> False

PlutusTx.unstableMakeIsData ''Payout
PlutusTx.unstableMakeIsData ''Swap
PlutusTx.unstableMakeIsData ''Redeemer

-------------------------------------------------------------------------------
-- Validation
-------------------------------------------------------------------------------
-- check that each user is paid
-- and the total is correct
validateOutputConstraints :: SwapTxInfo -> Map PubKeyHash Value -> Bool
validateOutputConstraints info constraints = all (uncurry (paidAtleastTo info)) $ M.toList constraints

-- Every branch but user initiated cancel requires checking the input
-- to ensure there is only one script input.
swapValidator :: Swap -> Redeemer -> SwapScriptContext -> Bool
swapValidator s r ctx =
  let
    info :: SwapTxInfo
    info = aScriptContextTxInfo ctx

    singleSigner :: PubKeyHash
    singleSigner = case atxInfoSignatories info of
      [x] -> x
      _ -> TRACE_ERROR("single signer expected")

    -- Verify that the script inputs are all for this script
    validScriptInputs :: Bool
    validScriptInputs =
      let
        isScriptInput :: SwapTxInInfo -> Bool
        isScriptInput = isScriptAddress . atxOutAddress . atxInInfoResolved
      in any (not . isScriptInput) . atxInfoInputs $ info

    isBeforeDeadline :: Bool
    isBeforeDeadline = case sDeadline s of
      Nothing -> True
      Just d -> d `after` atxInfoValidRange info

    isAfterDeadline :: Bool
    isAfterDeadline = case sDeadline s of
      Nothing -> False
      Just d -> d `before` atxInfoValidRange info


    convertDatum :: Datum -> Swap
    convertDatum d = case PlutusTx.fromBuiltinData . getDatum $ d of
      Nothing -> TRACE_ERROR("found datum that is not a swap")
      Just !x -> x

    allSwaps :: [Swap]
    allSwaps = fmap (convertDatum . snd) . atxInfoData $ info

    outputsAreValid :: [Payout] -> Bool
    outputsAreValid = validateOutputConstraints info . mergeAll . map (uncurry M.singleton . payoutToInequality)
  in TRACE_IF_FALSE("invalid script inputs", validScriptInputs) && case r of
    Cancel -> TRACE_IF_FALSE("not signed by owner", (singleSigner == sOwner s))
    Close -> case sDeadline s of
      Nothing -> TRACE_ERROR("swap has no deadline")
      Just _ ->
        let
          -- assume all redeemers are Close and all the assets are going back to their owner
          assetsReturnedToOwner :: Bool
          assetsReturnedToOwner = outputsAreValid . flip fmap allSwaps $ \s' -> Payout (sOwner s') (sSwapValue s')
        in TRACE_IF_FALSE("before the deadline", isAfterDeadline) && TRACE_IF_FALSE("wrong output", assetsReturnedToOwner)
    Buy ->
      let
        -- assume all redeemers are buys, all the payouts should be paid, and the assets should go to the tx signer (aka the buyer)
        assets :: Value
        sellerPayouts :: [Payout]
        (assets, sellerPayouts) = flip foldMap allSwaps $ \s' -> (sSwapValue s', sSwapPayouts s')

        assetsPaidToBuyer :: Bool
        assetsPaidToBuyer = paidAtleastTo info singleSigner assets

        payoutsToSeller :: Bool
        payoutsToSeller = outputsAreValid sellerPayouts
      in TRACE_IF_FALSE("deadline passed", isBeforeDeadline)
        && TRACE_IF_FALSE("wrong buyer output", assetsPaidToBuyer)
        && TRACE_IF_FALSE("wrong payouts", payoutsToSeller)

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
