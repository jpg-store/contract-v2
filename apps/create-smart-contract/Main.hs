{-# LANGUAGE LambdaCase #-}

module Main where

import Cardano.Api hiding (TxId)
import Ledger (PubKeyHash)
import Options.Applicative

import Canonical.JpgStore.BulkPurchase

type Opts = (PubKeyHash, FilePath)

opts :: ParserInfo Opts
opts =
  let
    parseOpts = (,)
      <$> (argument str . mconcat $
        [ metavar "PKH"
        , help "Public key hash of person that can emergency close swaps"
        ])
      <*> (argument str . mconcat $
        [ metavar "FILE"
        , help "File to which the plutus script will be written"
        ])
  in info (parseOpts <**> helper) . mconcat $
    [ fullDesc
    , progDesc "Create a smart contract for bulk purchases"
    ]

main :: IO ()
main = execParser opts >>= \(pkh, filePath) -> do
  writeFileTextEnvelope filePath Nothing (swap pkh) >>= \case
    Left err -> print $ displayError err
    Right () -> putStrLn $ "wrote NFT validator to file " ++ filePath
