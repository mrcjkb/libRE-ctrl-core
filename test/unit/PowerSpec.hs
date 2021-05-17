module PowerSpec (powerSpecTests) where

import Control.Monad
import LibRECtrl.Core.Domain.Power
import LibRECtrl.Core.Domain.ProdCon
import LibRECtrl.Core.Domain.Unit
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Arbitrary

powerSpecTests =
  [ testGroup
      "PowerUnit tests"
      [ testProperty "SI unit should always be W" siUnitAlwaysWProperty,
        testProperty "SI offset should always be 0" offsetAlwaysZeroProperty,
        testProperty "SI factor should alays be greater than or equal to 1 for non-user-defined PowerUnits" factorGreaterThanOrEqualToOneProperty,
        testProperty "Ordering should be based on SI factor" orderBasedOnSiFactorProperty
      ],
    testGroup
      "Power tests"
      [ testProperty "Adding Powers of arbitrary units should result in value with greater unit" additionWithArbitraryUnitsProperty,
        testProperty "Subtracting Powers of arbitrary units should result in value with greater unit" subtractionWithArbitraryUnitsProperty,
        testProperty "Adding Powers of arbitrary units results in the same as adding their SI values" additionResultsInCorrectSiValueProperty,
        testProperty "Subtracting Powers of arbitrary units results in the same as subtracting their SI values" subtractionResultsInCorrectSiValueProperty,
        testProperty "Dividing Power of arbitrary unit results in the same as dividing its SI value" divisionResultsInCorrectSiValueProperty,
        testProperty "Multiplying Power of arbitrary unit results in the same as multiplying its SI value" multiplicationResultsInCorrectSiValueProperty
      ],
    testGroup
      "Power function tests"
      [ testProperty "Producer surplus is always positive or zero." producerSurplusIsAlwaysPositiveOrZero,
        testProperty "Producer deficit is always negative or zero." producerDeficitIsAlwaysNegativeOrZero
      ]
  ]

siUnitAlwaysWProperty :: PowerUnit -> Bool
siUnitAlwaysWProperty x = si x == W

offsetAlwaysZeroProperty :: PowerUnit -> Bool
offsetAlwaysZeroProperty x = siOffset x == 0

factorGreaterThanOrEqualToOneProperty :: PowerUnit -> Bool
factorGreaterThanOrEqualToOneProperty UserDefinedPowerUnit {} = True
factorGreaterThanOrEqualToOneProperty x = siFactor x >= 1

orderBasedOnSiFactorProperty :: PowerUnit -> Bool
orderBasedOnSiFactorProperty x
  | siFactor x > siFactor GW = x > GW
  | siFactor x > siFactor MW = x > MW
  | siFactor x > siFactor KW = x > KW
  | siFactor x > 1 = x > W
  | siFactor x == 1 = x == W
  | otherwise = x < W

additionWithArbitraryUnitsProperty :: Double -> Bool
additionWithArbitraryUnitsProperty = operationOnValuesOfArbitraryUnitsProperty (+)

subtractionWithArbitraryUnitsProperty :: Double -> Bool
subtractionWithArbitraryUnitsProperty = operationOnValuesOfArbitraryUnitsProperty (-)

operationOnValuesOfArbitraryUnitsProperty :: (Power -> Power -> Power) -> Double -> Bool
operationOnValuesOfArbitraryUnitsProperty fun x
  | siFactor u > siFactor GW = unit (pv `fun` oneGW) == u
  | siFactor u <= siFactor GW = unit (pv `fun` oneGW) == GW
  | siFactor u <= siFactor MW = unit (pv `fun` oneMW) == MW
  | siFactor u <= siFactor KW = unit (pv `fun` oneKW) == KW
  | siFactor u <= siFactor W = unit (pv `fun` oneW) == W
  | otherwise = unit (pv `fun` oneW) == W
  where
    pv = Power x W
    u = unit pv
    oneGW = Power 1 GW
    oneMW = Power 1 MW
    oneKW = Power 1 KW
    oneW = Power 1 W

additionResultsInCorrectSiValueProperty :: PowerUnit -> Bool
additionResultsInCorrectSiValueProperty = operationResultsInCorrectSiValueProperty (+)

subtractionResultsInCorrectSiValueProperty :: PowerUnit -> Bool
subtractionResultsInCorrectSiValueProperty = operationResultsInCorrectSiValueProperty (-)

divisionResultsInCorrectSiValueProperty :: PowerUnit -> Bool
divisionResultsInCorrectSiValueProperty = operationResultsInCorrectSiValueProperty (/)

multiplicationResultsInCorrectSiValueProperty :: PowerUnit -> Bool
multiplicationResultsInCorrectSiValueProperty = operationResultsInCorrectSiValueProperty (*)

-- FIXME: Limit range of values, as double precision can be an issue for very large values
operationResultsInCorrectSiValueProperty :: (Power -> Power -> Power) -> PowerUnit -> Bool
operationResultsInCorrectSiValueProperty fun pu = difference < precision
  where
    oneGW = Power 1 GW
    pv = Power 1 pu
    resultOfOperationOnRawValues = pv `fun` oneGW
    resultOfOperationOnSiValues = toSi pv `fun` toSi oneGW
    difference = abs $ resultOfOperationOnSiValues - resultOfOperationOnRawValues
    precision = 1e-5

producerSurplusIsAlwaysPositiveOrZero :: ProdConPowerPair -> Bool
producerSurplusIsAlwaysPositiveOrZero = prodConPowerBalanceMatchesPredicate producerSurplus (>= 0)

producerDeficitIsAlwaysNegativeOrZero :: ProdConPowerPair -> Bool
producerDeficitIsAlwaysNegativeOrZero = prodConPowerBalanceMatchesPredicate producerDeficit (<= 0)

prodConPowerBalanceMatchesPredicate :: (ProdCon Power -> ProdCon Power -> ProdCon Power) -> (Power -> Bool) -> ProdConPowerPair -> Bool
prodConPowerBalanceMatchesPredicate balanceFunc predicate (ProdConPowerPair x y) = predicate result
  where
    balance = balanceFunc x y
    result = rawValue balance

instance Arbitrary PowerUnit where
  arbitrary = createArbitraryPowerUnit

data ProdConPowerPair = ProdConPowerPair
  { prod :: ProdCon Power,
    con :: ProdCon Power
  }
  deriving (Show)

instance Arbitrary ProdConPowerPair where
  arbitrary = do
    producerValue <- arbitrarySizedFractional
    consumerValue <- arbitrarySizedFractional
    let producer = Production $ Power {value = producerValue, unit = W}
    let consumer = Consumption $ Power {value = consumerValue, unit = W}
    return ProdConPowerPair {prod = producer, con = consumer}

-- | Helper Enum for PowerUnit's Arbitrary instance
data PowerUnitBoundedEnum = W' | KW' | MW' | GW' | UserDefined' deriving (Eq, Enum, Bounded)

-- | Helper function for creating a power unit from an arbitrary Enum
createPowerUnit :: PowerUnitBoundedEnum -> PowerUnit
createPowerUnit W' = W
createPowerUnit KW' = KW
createPowerUnit MW' = MW
createPowerUnit GW' = GW
createPowerUnit UserDefined' = error "Cannot create user defined unit from PowerUnitBoundedEnum."

-- | Helper action for creating an arbitrary power unit.
createArbitraryPowerUnit :: Gen PowerUnit
createArbitraryPowerUnit = do
  powerUnitBoundedEnum <- arbitraryBoundedEnum :: Gen PowerUnitBoundedEnum
  arbitraryUserDefinedPowerUnit <- createArbitraryUserDefinedPowerUnit
  let powerUnit
        | powerUnitBoundedEnum == UserDefined' = arbitraryUserDefinedPowerUnit
        | otherwise = createPowerUnit powerUnitBoundedEnum
  return powerUnit

-- | Helper action for creating an arbitrary user defined power unit
createArbitraryUserDefinedPowerUnit :: Gen PowerUnit
createArbitraryUserDefinedPowerUnit = do
  base <- createArbitraryPowerUnit
  f <- arbitrarySizedFractional
  name <- arbitraryUnicodeChar
  return
    UserDefinedPowerUnit
      { baseUnit = base,
        conversionFactor = f,
        unitName = [name]
      }
