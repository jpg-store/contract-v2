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
import           Ledger
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
        ( FROM_BUILT_IN_DATA("datum failed", "-1", a))
        ( FROM_BUILT_IN_DATA("redeemer failed", "-2", b))
        ( FROM_BUILT_IN_DATA("script context failed", "-3", c))
    )
