import ProdConSpec(prodConSpecTests)
import PowerSpec(powerSpecTests)
import Test.Framework (defaultMain)

main :: IO ()
main = defaultMain tests
  where
    tests = prodConSpecTests ++ powerSpecTests

