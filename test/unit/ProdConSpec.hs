{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module ProdConSpec (prodConSpecTests) where

import LibRECtrl.Core.Domain.ProdCon
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Arbitrary

prodConSpecTests =
  [ testGroup
      "ProdCon Functor tests"
      [ testProperty "fmap on ProdCon does not alter type." fmapOnProdConResultsInSameType ],
    testGroup
      "ProdCon Applicative tests"
      [],
    testGroup
      "ProdCon Monad tests"
      []
  ]

type DProdCon = ProdCon Double

fmapOnProdConResultsInSameType :: DProdCon -> Bool
fmapOnProdConResultsInSameType (Production p) = fmapOnProductionResultsInProduction $ Production p
fmapOnProdConResultsInSameType (Consumption c) = fmapOnConsumptionResultsInConsumption $ Consumption c
fmapOnProdConResultsInSameType b = fmapOnBalanceResultsInBalance b

fmapOnProductionResultsInProduction :: DProdCon -> Bool
fmapOnProductionResultsInProduction production = case result of
  Production _ -> True
  _ -> False
  where
    result = fmap (* 2) production

fmapOnConsumptionResultsInConsumption :: DProdCon -> Bool
fmapOnConsumptionResultsInConsumption consumption = case result of
  Consumption _ -> True
  _ -> False
  where
    result = fmap (* 2) consumption

fmapOnBalanceResultsInBalance :: DProdCon -> Bool
fmapOnBalanceResultsInBalance balance = case result of
  Balance _ -> True
  _ -> False
  where
    result = fmap (* 2) balance

instance Arbitrary DProdCon where
  arbitrary = do
    value <- arbitrarySizedFractional
    prodConType <- arbitraryBoundedEnum :: Gen ProdConBoundedEnum
    return case prodConType of
      Prod -> Production value
      Con -> Consumption value
      Bal -> Balance value

-- | Helper Enum for ProdCon's Arbitrary instance
data ProdConBoundedEnum = Prod | Con | Bal deriving (Eq, Enum, Bounded)
