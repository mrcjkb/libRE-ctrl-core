module ProdConSpec(prodConSpecTests) where

import LibRECtrl.Core.Domain.ProdCon
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Arbitrary

prodConSpecTests =
  [ testGroup
      "ProdCon Functor tests"
      [
      -- TODO
      ],
    testGroup
      "ProdCon Applicative tests"
      [
      ],
    testGroup
      "ProdCon Monad tests"
      [
      ]
  ]
