{-# LANGUAGE NoImplicitPrelude #-}
module Canonical.JpgStore.NftMinter where
import Canonical.Shared
import Cardano.Api.Shelley (PlutusScript(..), PlutusScriptV2)
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as BSS
import           Plutus.V2.Ledger.Contexts
import           Plutus.V1.Ledger.Scripts
import           Plutus.V2.Ledger.Tx
import           Plutus.V1.Ledger.Crypto
import           Plutus.V1.Ledger.Address
import Plutus.V1.Ledger.Value
import PlutusTx
import qualified PlutusTx.AssocMap as M
import PlutusTx.Prelude



type WrappedMintingPolicyType = BuiltinData -> BuiltinData -> ()

data NftConfig = NftConfig
  { ncInitialUtxo :: TxOutRef
  , ncTokenName   :: TokenName
  }

data NonEmptyAddress = NonEmptyAddress
  { neaHead :: Address
  , nealTail :: [Address]
  }

{-# INLINABLE nonEmptyAddressToList #-}
nonEmptyAddressToList :: NonEmptyAddress -> [Address]
nonEmptyAddressToList (NonEmptyAddress x xs) = x : xs

data NonEmptyPubKeyHash = NonEmptyPubKeyHash
  { nepkhHead :: PubKeyHash
  , nepkhTail :: [PubKeyHash]
  }

{-# INLINABLE nonEmptyPubKeyHashToList #-}
nonEmptyPubKeyHashToList :: NonEmptyPubKeyHash -> [PubKeyHash]
nonEmptyPubKeyHashToList (NonEmptyPubKeyHash x xs) = x : xs

data SwapDynamicConfig = SwapDynamicConfig
  { sdcValidOutputAddresses :: NonEmptyAddress
  -- ^ change this to addresses
  , sdcMarketplacePkhs :: NonEmptyPubKeyHash
  }

unstableMakeIsData ''NonEmptyAddress
unstableMakeIsData ''NonEmptyPubKeyHash
unstableMakeIsData ''SwapDynamicConfig
makeLift ''NftConfig

mkNftMinter :: NftConfig -> BuiltinData -> ScriptContext -> Bool
mkNftMinter NftConfig {..} _ ScriptContext
  { scriptContextTxInfo = TxInfo {..}
  , scriptContextPurpose = Minting thisCurrencySymbol
  } =
  let
    hasWitness :: Value -> Bool
    hasWitness (Value v) = case M.lookup thisCurrencySymbol v of
      Just m -> case M.toList m of
        [(_, c)] -> if c == 1 then True else traceError "wrong token count"
        _ -> traceError "wrong number of tokens with policy id"
      _ -> False

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == ncInitialUtxo) txInfoInputs

    -- This errors if more than one token is used as an output with this policy id
    _newOutput :: SwapDynamicConfig
    _newOutput = case filter (\TxOut {..} -> hasWitness txOutValue) txInfoOutputs of
      [ TxOut { txOutDatum }
        ] -> case txOutDatum of
          OutputDatum (Datum dbs) -> unsafeFromBuiltinData dbs
          OutputDatumHash dh0 -> extractData (M.toList txInfoData) dh0
          NoOutputDatum -> traceError "Script output missing datum"
      _ -> traceError "Impossible. No minted output."

    onlyOneTokenMinted :: Bool
    onlyOneTokenMinted =
      hasSingleToken
        txInfoMint
        thisCurrencySymbol
        ncTokenName

  in traceIfFalse "Missing significant UTxO!" hasUTxO
  && traceIfFalse "Wrong mint amount!" onlyOneTokenMinted

mkNftMinter _ _ _ = traceError "wrong type of script purpose!"

wrappedPolicy :: NftConfig -> WrappedMintingPolicyType
wrappedPolicy = wrapMint . mkNftMinter

policy :: NftConfig -> MintingPolicy
policy cfg = mkMintingPolicyScript $
  $$(compile [|| \c -> wrappedPolicy c ||])
  `PlutusTx.applyCode`
  PlutusTx.liftCode cfg

plutusScript :: NftConfig -> Script
plutusScript = unMintingPolicyScript . policy

validator :: NftConfig -> Validator
validator = Validator . plutusScript

nftMinterPolicyId :: NftConfig -> CurrencySymbol
nftMinterPolicyId = mpsSymbol . mintingPolicyHash . policy

scriptAsCbor :: NftConfig -> BSL.ByteString
scriptAsCbor = serialise . validator

nftMinter :: NftConfig -> PlutusScript PlutusScriptV2
nftMinter
  = PlutusScriptSerialised
  . BSS.toShort
  . BSL.toStrict
  . scriptAsCbor
