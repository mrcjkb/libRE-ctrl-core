import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.QuickCheck.Arbitrary

import Control.Monad

import LibRECtrl.Core.Domain.Unit
import LibRECtrl.Core.Domain.Power

main :: IO ()
main = defaultMain tests

tests = [
        testGroup "PowerUnitTestGroup" [
                testProperty "SI unit should always be W." siUnitAlwaysWProperty
              , testProperty "SI offset should always be 0." offsetAlwaysZeroProperty
              , testProperty "SI factor should alays be greater than or equal to 1 for non-user-defined PowerUnits." factorGreaterThanOrEqualToOneProperty
              , testProperty "Ordering should be based on SI factor." orderBasedOnSiFactorProperty
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

instance Arbitrary PowerUnit where
  arbitrary = createArbitraryPowerUnit

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
  let powerUnit | powerUnitBoundedEnum == UserDefined' = arbitraryUserDefinedPowerUnit
                | otherwise = createPowerUnit powerUnitBoundedEnum
  return powerUnit

-- | Helper action for creating an arbitrary user defined power unit
createArbitraryUserDefinedPowerUnit :: Gen PowerUnit
createArbitraryUserDefinedPowerUnit = do
  base <- createArbitraryPowerUnit
  f <- arbitrarySizedFractional
  name <- arbitraryUnicodeChar
  return UserDefinedPowerUnit {
            baseUnit = base,
            conversionFactor = f,
            unitName = [name] 
          }
