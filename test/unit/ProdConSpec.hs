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
      [ testProperty "<$> on ProdCon does not alter type" fmapOnProdConResultsInSameType],
    testGroup
      "ProdCon Applicative tests"
      [ testProperty "<*> on ProdCon with Production results in Balance" appOnProdConWithProductionResultsInBalance,
        testProperty "<*> on ProdCon with Consumption results in Balance" appOnProdConWithConsumptionResultsInBalance,
        testProperty "<*> on ProdCon with Balance results in Balance" appOnProdConWithBalanceResultsInBalance
      ],
    testGroup
      "ProdCon Monad tests"
      [ testProperty "Monad actions result in Balance" monadActionsResultInBalance ]
  ]

type DProdCon = ProdCon Double

fmapOnProdConResultsInSameType :: DProdCon -> Bool
fmapOnProdConResultsInSameType (Production p) = fmapOnProductionResultsInProduction $ Production p
fmapOnProdConResultsInSameType (Consumption c) = fmapOnConsumptionResultsInConsumption $ Consumption c
fmapOnProdConResultsInSameType b = fmapOnBalanceResultsInBalance b

fmapOnProductionResultsInProduction :: DProdCon -> Bool
fmapOnProductionResultsInProduction production = isProduction result
  where
    result = fmap (* 2) production

fmapOnConsumptionResultsInConsumption :: DProdCon -> Bool
fmapOnConsumptionResultsInConsumption consumption = isConsumption result
  where
    result = fmap (* 2) consumption

fmapOnBalanceResultsInBalance :: DProdCon -> Bool
fmapOnBalanceResultsInBalance balance = isBalance result
  where
    result = fmap (* 2) balance

appOnProdConWithProductionResultsInBalance :: DProdCon -> Bool
appOnProdConWithProductionResultsInBalance = appOnProdConResultsInBalance $ Production 0

appOnProdConWithConsumptionResultsInBalance :: DProdCon -> Bool
appOnProdConWithConsumptionResultsInBalance = appOnProdConResultsInBalance $ Consumption 0

appOnProdConWithBalanceResultsInBalance :: DProdCon -> Bool
appOnProdConWithBalanceResultsInBalance = appOnProdConResultsInBalance $ Balance 0

appOnProdConResultsInBalance :: DProdCon -> DProdCon -> Bool
appOnProdConResultsInBalance x y = isBalance result
  where
    result :: DProdCon
    result = (+) <$> x <*> y

monadActionsResultInBalance :: DProdCon -> Bool
monadActionsResultInBalance = isBalance . calculateBalance

calculateBalance :: DProdCon -> DProdCon
calculateBalance x = do
  x' <- x
  y <- getConsumption 1
  z <- getProduction 2
  return $ x' - y - z

getProduction :: Double -> DProdCon
getProduction = Production

getConsumption :: Double -> DProdCon
getConsumption = Consumption


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
