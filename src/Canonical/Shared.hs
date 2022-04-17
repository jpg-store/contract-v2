{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Canonical.Shared where
import           PlutusTx.Prelude
import           PlutusTx
import           Ledger
import           Plutus.V1.Ledger.Credential
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

{-# INLINABLE extractDatumBytes #-}
extractDatumBytes :: [(DatumHash, Datum)] -> DatumHash -> BuiltinData
extractDatumBytes datums dh = getDatum $ extractDatum datums dh

{-# INLINABLE extractDatum #-}
extractDatum :: [(DatumHash, Datum)] -> DatumHash -> Datum
extractDatum datums dh = go datums where
  go = \case
    [] -> TRACE_ERROR("Failed to find datum")
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
  in FROM_BUILT_IN_DATA("extractData failed", a)

{-# INLINABLE convertInputs #-}
convertInputs
  :: UnsafeFromData a
  => [TxInInfo]
  -> [(DatumHash, Datum)]
  -> ValidatorHash
  -> [(a, Value)]
convertInputs ins datums vh = go [] ins  where
  go acc = \case
    [] -> acc
    TxInInfo
      {txInInfoResolved = TxOut
        { txOutDatumHash = mdh
        , txOutAddress = Address {..}
        , txOutValue
        }
      }:xs ->

        if ScriptCredential vh == addressCredential then
          case mdh of
            Just dh ->
              go  ( ( unsafeFromBuiltinData (extractDatumBytes datums dh)
                    , txOutValue
                    )
                  : acc
                  )
                  xs
            Nothing -> TRACE_ERROR("Script input missing datum hash")
        else
          go acc xs

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
        ( FROM_BUILT_IN_DATA("datum failed", a))
        ( FROM_BUILT_IN_DATA("redeemer failed", b))
        ( FROM_BUILT_IN_DATA("script context failed", c))
    )

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
        ( FROM_BUILT_IN_DATA("redeemer failed", a))
        ( FROM_BUILT_IN_DATA("script context failed", b))
    )
