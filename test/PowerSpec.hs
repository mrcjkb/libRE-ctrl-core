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
           ]
      ]

siUnitAlwaysWProperty :: PowerUnit -> Bool
siUnitAlwaysWProperty x = si x == W

offsetAlwaysZeroProperty :: PowerUnit -> Bool
offsetAlwaysZeroProperty x = siOffset x == 0  

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
createPowerUnit UserDefined' = error "Cannot create user defined unit from enum."

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
