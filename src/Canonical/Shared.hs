{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
module Canonical.Shared where
import           PlutusTx.Prelude
import           PlutusTx
import           Plutus.V1.Ledger.Crypto
import           Plutus.V1.Ledger.Time
import           Plutus.V1.Ledger.Scripts
import           Plutus.V1.Ledger.Value
import qualified Cardano.Api.Shelley as Shelly
import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString.Lazy as BSL
import           Codec.Serialise
import qualified PlutusTx.AssocMap as M
#include "DebugUtilities.h"

data SwapConfig = SwapConfig
  { scMarketplaceFee     :: Integer
  , scConfigNftPolicyId  :: CurrencySymbol
  , scConfigNftTokenName :: TokenName
  }

data NonEmptyPubKeyHashList = NonEmptyPubKeyHashList
  { nepkhlHead :: PubKeyHash
  , nepkhlTail :: [PubKeyHash]
  }

data SwapDynamicConfig = SwapDynamicConfig
  { sdcValidOutputPkhs :: NonEmptyPubKeyHashList
  , sdcMarketplacePkhs :: NonEmptyPubKeyHashList
  }

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

{-# INLINABLE extractDatumBytes #-}
extractDatumBytes :: [(DatumHash, Datum)] -> DatumHash -> BuiltinData
extractDatumBytes datums dh = getDatum $ extractDatum datums dh

{-# INLINABLE extractDatum #-}
extractDatum :: [(DatumHash, Datum)] -> DatumHash -> Datum
extractDatum datums dh = go datums where
  go = \case
    [] -> TRACE_ERROR("Failed to find datum", "-2")
    (x, y):xs ->
      if x == dh then
        y
      else
        go xs

{-# INLINABLE extractData #-}
extractData :: forall a. DataConstraint(a) => [(DatumHash, Datum)] -> DatumHash -> a
extractData ds dh =
  let
    a = extractDatumBytes ds dh
  in FROM_BUILT_IN_DATA("extractData failed", "-5", a, a)

{-# INLINABLE hasSingleToken #-}
hasSingleToken :: Value -> CurrencySymbol -> TokenName -> Bool
hasSingleToken (Value v) s t = case M.lookup s v of
  Just m -> case M.toList m of
    [(t', c)] -> t' == t && c == 1
    _ -> traceError "wrong number of tokens with policy id"
  _ -> False

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

mintingPolicyHash :: MintingPolicy -> MintingPolicyHash
mintingPolicyHash
  = MintingPolicyHash
  . getScriptHash
  . scriptHash
  . getValidator
  . Validator
  . getMintingPolicy

wrapMint  :: forall a b .
            ( DataConstraint(a)
            , DataConstraint(b)
            )
      => (a -> b -> Bool)
      -> BuiltinData
      -> BuiltinData
      -> ()
wrapMint f a b
  = check
    ( f
        ( FROM_BUILT_IN_DATA("redeemer failed", "-1", a, a))
        ( FROM_BUILT_IN_DATA("script context failed", "-2", b, b))
    )

makeLift ''SwapConfig
unstableMakeIsData ''NonEmptyPubKeyHashList
unstableMakeIsData ''SwapDynamicConfig
