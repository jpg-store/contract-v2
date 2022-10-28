{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import           Options.Generic
import           Canonical.JpgStore.BulkPurchase
import           Canonical.Shared
import           Canonical.JpgStore.NftMinter
import           Plutus.V1.Ledger.Value
import           Plutus.V1.Ledger.Tx
import           Data.String
import           Cardano.Api hiding (TxId)
import           Plutus.V1.Ledger.Bytes

instance Read TokenName where
  readsPrec _ x = [(fromString x, "")]
instance ParseRecord TokenName where
  parseRecord = fmap getOnly parseRecord
instance ParseField TokenName
instance ParseFields TokenName

instance Read TxOutRef where
  readsPrec _ s =
    let
      (x, y) = span (/= '#') s
    in
      [(TxOutRef (TxId $ getLedgerBytes $ fromString x) $ read $ tail y, "")]
instance ParseRecord TxOutRef where
  parseRecord = fmap getOnly parseRecord
instance ParseField TxOutRef
instance ParseFields TxOutRef

data Options = Options
  { swapOutput        :: FilePath
  , nftOutput         :: FilePath
  , nftPolicyIdOutput :: FilePath
  , marketPlaceFee    :: Integer
  , nftUtxo           :: TxOutRef
  , nftTokenName      :: TokenName
  } deriving stock (Eq, Show, Generic)

instance ParseRecord Options where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

main :: IO ()
main = run =<< getRecord "jpg.store swap contract compiler"

run :: Options -> IO ()
run Options {..} = do
  let nftConfig = NftConfig
        { ncInitialUtxo = nftUtxo
        , ncTokenName   = nftTokenName
        }

  writeFileTextEnvelope nftOutput Nothing (nftMinter nftConfig) >>= \case
      Left err -> print $ displayError err
      Right () -> putStrLn $ "wrote validator to file " ++ nftOutput

  let theNftPolicyId = nftMinterPolicyId nftConfig

  writeFile nftPolicyIdOutput $ show theNftPolicyId

  let swapConfig = SwapConfig
        { scMarketplaceFee    = marketPlaceFee
        , scConfigNftPolicyId = theNftPolicyId
        , scConfigNftTokenName = nftTokenName
        }

  writePlutusFile swapConfig swapOutput
