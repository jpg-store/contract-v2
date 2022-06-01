import Test.Hspec
import Prelude
import Canonical.JpgStore.BulkPurchase
import qualified PlutusTx.AssocMap as A
import Plutus.V1.Ledger.Ada
import Plutus.V1.Ledger.Value

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "satisfyExpectations" $ do
  it "works if empty" $ shouldSatisfy mempty (satisfyExpectations A.empty)
  it "works for a single policy and any token" $ do
    let theValue = Value $ A.fromList
          [ (adaSymbol, A.singleton adaToken 1000)
          , ("123456", A.fromList
                [ ("6789", 10000)
                , ("1234", 10000)
                ]
            )
          ]

        theExpectations
          = A.singleton "123456" (Natural 1, A.empty)
    shouldSatisfy theValue (satisfyExpectations theExpectations)

  it "works for a single policy and single token" $ do
    let theValue = Value $ A.fromList
          [ (adaSymbol, A.singleton adaToken 1000)
          , ("123456", A.fromList
                [ ("6789", 10000)
                , ("1234", 10000)
                ]
            )
          ]

        theExpectations
          = A.singleton "123456" (Natural 0, A.singleton "6789" $ WholeNumber 10000)
    shouldSatisfy theValue (satisfyExpectations theExpectations)

  it "works for a single policy, with a specific token and any token" $ do
    let theValue = Value $ A.fromList
          [ (adaSymbol, A.singleton adaToken 1000)
          , ("123456", A.fromList
                [ ("6789", 10000)
                , ("1234", 10000)
                ]
            )
          ]

        theExpectations
          = A.singleton "123456" (Natural 1, A.singleton "6789" $ WholeNumber 10000)
    shouldSatisfy theValue (satisfyExpectations theExpectations)

  it "works for a multiple policies, with a specific tokens and any token" $ do
    let theValue = Value $ A.fromList
          [ (adaSymbol, A.singleton adaToken 1000)
          , ("123456", A.fromList
                [ ("6789", 10000)
                , ("1234", 10000)
                ]
            )
          ]

        theExpectations = A.fromList
          [ ("123456", (Natural 1, A.singleton "1234" $ WholeNumber 10000))
          , (adaSymbol, (Natural 0, A.singleton adaToken $ WholeNumber 100))
          ]
    shouldSatisfy theValue (satisfyExpectations theExpectations)

  it "works for a multiple policies, with a specific tokens and any token if not all of the specific tokens match" $ do
    let theValue = Value $ A.fromList
          [ (adaSymbol, A.singleton adaToken 1000)
          , ("123456", A.fromList
                [ ("1234", 10000)
                ]
            )
          ]

        theExpectations = A.fromList
          [ ("123456", (Natural 1, A.singleton "1234" $ WholeNumber 9999))
          , (adaSymbol, (Natural 0, A.singleton adaToken $ WholeNumber 100))
          ]
    shouldSatisfy theValue (satisfyExpectations theExpectations)

  it "does not work for a multiple policies, with a specific tokens and negative token amounts" $ do
    let theValue = Value $ A.fromList
          [ (adaSymbol, A.singleton adaToken 1000)
          , ("123456", A.fromList
                [ ("6789", (-10000))
                , ("1234", 10000)
                ]
            )
          ]

        theExpectations = A.fromList
          [ ("123456", (Natural 1, A.singleton "1234" $ WholeNumber 10000))
          , (adaSymbol, (Natural 0, A.singleton adaToken $ WholeNumber 100))
          ]
    shouldSatisfy theValue (not . satisfyExpectations theExpectations)

  it "does not work for a multiple policies, with a specific tokens and negative token amounts that are too low" $ do
    let theValue = Value $ A.fromList
          [ (adaSymbol, A.singleton adaToken 1000)
          , ("123456", A.fromList
                [ ("6789", (-1))
                , ("6788", 2)
                , ("1234", 10000)
                ]
            )
          ]

        theExpectations = A.fromList
          [ ("123456", (Natural 3, A.singleton "1234" $ WholeNumber 9999))
          , (adaSymbol, (Natural 0, A.singleton adaToken $ WholeNumber 100))
          ]
    shouldSatisfy theValue (not . satisfyExpectations theExpectations)

  it "does work for a multiple policies, with a specific tokens and negative token amounts that are not too low" $ do
    let theValue = Value $ A.fromList
          [ (adaSymbol, A.singleton adaToken 1000)
          , ("123456", A.fromList
                [ ("6789", (-1))
                , ("6788", 3)
                , ("1234", 10000)
                ]
            )
          ]

        theExpectations = A.fromList
          [ ("123456", (Natural 3, A.singleton "1234" $ WholeNumber 9999))
          , (adaSymbol, (Natural 0, A.singleton adaToken $ WholeNumber 100))
          ]
    shouldSatisfy theValue (satisfyExpectations theExpectations)

  it "works for a multiple policies, with a specific tokens and any token, fails if one of the tokens is missing" $ do
    let theValue = Value $ A.fromList
          [ (adaSymbol, A.singleton adaToken 1000)
          , ("123456", A.fromList
                [ ("6789", 10000)
                , ("1234", 10000)
                ]
            )
          ]

        theExpectations = A.fromList
          [ ("123456", (Natural 1, A.singleton "1234" $ WholeNumber 9999))
          , (adaSymbol, (Natural 0, A.singleton adaToken $ WholeNumber 100))
          , ("5673", (Natural 0, A.singleton "1234" $ WholeNumber 9999))
          ]
    shouldSatisfy theValue (not . satisfyExpectations theExpectations)
