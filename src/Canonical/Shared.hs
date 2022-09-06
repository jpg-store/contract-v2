{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Canonical.Shared where
import           PlutusTx.Prelude
import           PlutusTx
import           Plutus.V1.Ledger.Crypto
import           Plutus.V1.Ledger.Time
import           Plutus.V1.Ledger.Scripts
import qualified Cardano.Api.Shelley as Shelly
import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString.Lazy as BSL
import           Codec.Serialise
#include "DebugUtilities.h"

data Bid = Bid
  { bidBidder         :: PubKeyHash
  , bidAmount         :: Integer
  , bidTime           :: POSIXTime
  }

instance Eq Bid where
  {-# INLINABLE (==) #-}
  x == y
    =  (bidBidder x == bidBidder y)
    && (bidAmount x == bidAmount y)
    && (bidTime   x == bidTime   y)

PlutusTx.unstableMakeIsData ''Bid

wrap  :: forall a b c .
            ( DataConstraint(a)
            , DataConstraint(b)
            , DataConstraint(c)
            )
      => (a -> b -> c -> Bool)
      -> BuiltinData
      -> BuiltinData
      -> BuiltinData
      -> ()
wrap f a b c
  = check
    ( f
        ( FROM_BUILT_IN_DATA("datum failed", "-1", a, a))
        ( FROM_BUILT_IN_DATA("redeemer failed", "-2", b, b))
        ( FROM_BUILT_IN_DATA("script context failed", "-3", c, c))
    )

validatorHash :: Validator -> ValidatorHash
validatorHash = ValidatorHash . getScriptHash . scriptHash . getValidator

toCardanoApiScript :: Script -> Shelly.Script Shelly.PlutusScriptV2
toCardanoApiScript
  = Shelly.PlutusScript Shelly.PlutusScriptV2
  . Shelly.PlutusScriptSerialised
  . BSS.toShort
  . BSL.toStrict
  . serialise

scriptHash :: Script -> ScriptHash
scriptHash =
    ScriptHash
    . toBuiltin
    . Shelly.serialiseToRawBytes
    . Shelly.hashScript
    . toCardanoApiScript
