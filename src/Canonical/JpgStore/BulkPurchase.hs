{-# LANGUAGE BangPatterns #-}
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
  ( Swap(..)
  , swap
  ) where

import Cardano.Api.Shelley (PlutusScript(..), PlutusScriptV1)
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as SBS
import Ledger (Address(..), Datum(..), POSIXTime, PubKeyHash, after, before)
import Ledger.Typed.Scripts
import Plutus.V1.Ledger.Contexts
import Plutus.V1.Ledger.Credential
import Plutus.V1.Ledger.Value
import PlutusTx
import qualified PlutusTx.AssocMap as M
import PlutusTx.AssocMap (Map)
import PlutusTx.Prelude

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
  | EmergencyCancel
  | Close

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------
isScriptAddress :: Address -> Bool
isScriptAddress Address { addressCredential } = case addressCredential of
  ScriptCredential _ -> True
  _ -> False

-- Verify that there is only one script input and get it's value.
getOnlyScriptInput :: TxInfo -> Value
getOnlyScriptInput info =
  let
    isScriptInput :: TxInInfo -> Bool
    isScriptInput = isScriptAddress . txOutAddress . txInInfoResolved

    input = case filter isScriptInput . txInfoInputs $ info of
      [i] -> i
      _ -> traceError "expected exactly one script input"
  in txOutValue . txInInfoResolved $ input

payoutToInequality :: Payout -> (PubKeyHash, Value)
payoutToInequality Payout {..} = (pAddress, pValue)

mergePayoutsValue :: [Payout] -> Value
mergePayoutsValue = foldr (\x acc -> pValue x <> acc) mempty

paidAtleastTo :: TxInfo -> PubKeyHash -> Value -> Bool
paidAtleastTo info pkh val = valuePaidTo info pkh `geq` val

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
    (EmergencyCancel, EmergencyCancel) -> True
    (EmergencyCancel, _) -> False
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
validateOutputConstraints :: TxInfo -> Map PubKeyHash Value -> Bool
validateOutputConstraints info constraints = all (uncurry (paidAtleastTo info)) $ M.toList constraints

-- Every branch but user initiated cancel requires checking the input
-- to ensure there is only one script input.
swapValidator :: PubKeyHash -> Swap -> Redeemer -> ScriptContext -> Bool
swapValidator emergencyPkh s r ctx =
  let
    info :: TxInfo
    info = scriptContextTxInfo ctx

    singleSigner :: PubKeyHash
    singleSigner = case txInfoSignatories info of
      [x] -> x
      _ -> traceError "single signer expected"

    isBeforeDeadline :: Bool
    isBeforeDeadline = case sDeadline s of
      Nothing -> True
      Just d -> d `after` txInfoValidRange info

    isAfterDeadline :: Bool
    isAfterDeadline = case sDeadline s of
      Nothing -> False
      Just d -> d `before` txInfoValidRange info


    convertDatum :: Datum -> Swap
    convertDatum d = case PlutusTx.fromBuiltinData . getDatum $ d of
      Nothing -> traceError "found datum that is not a swap"
      Just !x -> x

    allSwaps :: [Swap]
    allSwaps = fmap (convertDatum . snd) . txInfoData $ info

    outputsAreValid :: [Payout] -> Bool
    outputsAreValid = validateOutputConstraints info . mergeAll . map (uncurry M.singleton . payoutToInequality)

    -- assume all redeemers are the same and all the assets are going back to their owner
    assetsReturnedToOwner :: Bool
    assetsReturnedToOwner = outputsAreValid . flip fmap allSwaps $ \s' -> Payout (sOwner s') (sSwapValue s')

    -- assume all redeemers are buys, all the payouts should be paid, and the assets should go to the tx signer (aka the buyer)
    assetsPaidToBuyer :: Bool
    assetsPaidToBuyer =
      outputsAreValid . flip concatMap allSwaps $ \s' -> Payout singleSigner (sSwapValue s') : sSwapPayouts s'
  in case r of
    Cancel ->
      traceIfFalse "deadline passed, use a close redeemer" isAfterDeadline
        && traceIfFalse "not signed by owner" (singleSigner == sOwner s)
        && traceIfFalse "wrong output" assetsReturnedToOwner
    EmergencyCancel ->
      traceIfFalse "not signed by emergency pub key hash" (singleSigner == emergencyPkh)
        && traceIfFalse "wrong output" assetsReturnedToOwner
    Close -> case sDeadline s of
      Nothing -> traceError "swap has no deadline"
      Just _ -> traceIfFalse "before the deadline" isAfterDeadline && traceIfFalse "wrong output" assetsReturnedToOwner
    Buy -> traceIfFalse "deadline passed" isBeforeDeadline && traceIfFalse "wrong output" assetsPaidToBuyer

-------------------------------------------------------------------------------
-- Entry Points
-------------------------------------------------------------------------------
data Swapping
instance ValidatorTypes Swapping where
  type DatumType Swapping = Swap
  type RedeemerType Swapping = Redeemer

validator :: PubKeyHash -> TypedValidator Swapping
validator pkh =
  mkTypedValidator @Swapping
    ($$(PlutusTx.compile [|| swapValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode pkh)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = wrapValidator

swap :: PubKeyHash -> PlutusScript PlutusScriptV1
swap = PlutusScriptSerialised . SBS.toShort . LB.toStrict . serialise . validatorScript . validator
