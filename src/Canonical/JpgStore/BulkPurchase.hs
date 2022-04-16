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
  ( Payout(..)
  , Redeemer(..)
  , Swap(..)
  , swap
  , writePlutusFile
  ) where

import qualified Cardano.Api as Api
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
import Prelude (IO, print, putStrLn)
import System.FilePath

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
isScriptAddress :: Address -> Bool
isScriptAddress Address { addressCredential } = case addressCredential of
  ScriptCredential _ -> True
  _ -> False

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
swapValidator :: Swap -> Redeemer -> ScriptContext -> Bool
swapValidator s r ctx =
  let
    info :: TxInfo
    info = scriptContextTxInfo ctx

    singleSigner :: PubKeyHash
    singleSigner = case txInfoSignatories info of
      [x] -> x
      _ -> traceError "single signer expected"

    -- Verify that the script inputs are all for this script
    validScriptInputs :: Bool
    validScriptInputs =
      let
        isScriptInput :: TxInInfo -> Bool
        isScriptInput = isScriptAddress . txOutAddress . txInInfoResolved
      in any (not . isScriptInput) . txInfoInputs $ info

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
  in traceIfFalse "invalid script inputs" validScriptInputs && case r of
    Cancel -> trace "cancel redeemer" traceIfFalse "not signed by owner" (singleSigner == sOwner s)
    Close -> trace "close redeemer" $ case sDeadline s of
      Nothing -> traceError "swap has no deadline"
      Just _ ->
        let
          -- assume all redeemers are Close and all the assets are going back to their owner
          assetsReturnedToOwner :: Bool
          assetsReturnedToOwner = outputsAreValid . flip fmap allSwaps $ \s' -> Payout (sOwner s') (sSwapValue s')
        in traceIfFalse "before the deadline" isAfterDeadline && traceIfFalse "wrong output" assetsReturnedToOwner
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
      in
        trace "buy redeemer" traceIfFalse "deadline passed" isBeforeDeadline
        && traceIfFalse "wrong buyer output" assetsPaidToBuyer
        && traceIfFalse "wrong payouts" payoutsToSeller

-------------------------------------------------------------------------------
-- Entry Points
-------------------------------------------------------------------------------
data Swapping
instance ValidatorTypes Swapping where
  type DatumType Swapping = Swap
  type RedeemerType Swapping = Redeemer

validator :: TypedValidator Swapping
validator =
  mkTypedValidator @Swapping
    $$(PlutusTx.compile [|| swapValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = wrapValidator

swap :: PlutusScript PlutusScriptV1
swap = PlutusScriptSerialised . SBS.toShort . LB.toStrict . serialise . validatorScript $ validator

writePlutusFile :: FilePath -> IO ()
writePlutusFile filePath = Api.writeFileTextEnvelope filePath Nothing swap >>= \case
  Left err -> print $ Api.displayError err
  Right () -> putStrLn $ "wrote NFT validator to file " ++ filePath
