{-#LANGUAGE GADTs#-}
module LibRECtrl.Core.Domain.Power(
      PowerUnit(..),
      PowerValue(..),
      convert -- TODO: Remove export
) where

import LibRECtrl.Core.Domain.Unit
import Numeric

-- | The power units supported by this library.
data PowerUnit = W | KW | MW | GW | UserDefinedPowerUnit {
                                      -- | The PowerUnit on which this user defined unit is based
                                      baseUnit :: PowerUnit
                                      -- | The conversion factor, used to convert this unit into its baseUnit
                                    , conversionFactor :: Double
                                      -- | The name of this unit
                                    , unitName :: String
                                    } deriving Eq

instance Show PowerUnit where
  show W = "W"
  show KW = "kW"
  show MW = "MW"
  show GW = "GW"
  show (UserDefinedPowerUnit _ _ name) = name

instance Ord PowerUnit where
   compare unit1 unit2 = compare' unit1 unit2 

instance Unit PowerUnit where
  si _ = W
  siFactor W = 1
  siFactor KW = 1000
  siFactor MW = 1000 * siFactor KW
  siFactor GW = 1000 * siFactor MW
  siFactor (UserDefinedPowerUnit baseUnit x _) = x * siFactor baseUnit
  siOffset _ = 0

-- | A PowerValue is a PhysicalValue that represents power.
-- | It comes with a Double value and a PowerUnit.
data PowerValue a b where
  PowerValue :: {
    value :: Double,
    unit :: PowerUnit
  } -> PowerValue a b

instance Show (PowerValue a b) where
  show (PowerValue x u) = mconcat [showFloat x "", " ", show u]

instance PhysicalValue (PowerValue a b) where
  toSi (PowerValue x u) = PowerValue {
                            value = siValue,
                            unit = si u
                          }
    where
      factor = siFactor u
      summand = siOffset u
      siValue = factor * x + summand


-- | Extract the SI value from a PowerValue
siValue :: PowerValue a b -> Double
siValue = value . toSi

-- | Determine the larger unit of two PowerValues
largerUnit :: PowerValue a b -> PowerValue a b -> PowerUnit
largerUnit pv1 pv2 = max (unit pv1) (unit pv2)

-- | Convert a PhysicalValue with a given unit to an equivalent PhysicalValue with another unit
convert :: PowerValue a b -> PowerUnit -> PowerValue a b
convert powerValue destUnit = PowerValue destValue destUnit
  where
    destValue = siValue powerValue / siFactor destUnit

-- | Factory to create a PowerValue in Watts from a Double value
toWatts :: Double -> PowerValue a b
toWatts x = PowerValue x W

instance Eq (PowerValue a b) where
  pv1 == pv2 = siValue pv1 == siValue pv2

instance Num (PowerValue a b) where
  pv1 + pv2 = convert siSum destUnit
    where
      siSum = toWatts $ siValue pv1 + siValue pv2
      destUnit = largerUnit pv1 pv2
  pv1 * pv2 = convert siProduct destUnit
    where
      siProduct = toWatts $ siValue pv1 * siValue pv2
      destUnit = largerUnit pv1 pv2
  abs (PowerValue x u) = PowerValue (abs x) u
  signum (PowerValue x u) = PowerValue (signum x) u
  fromInteger x = PowerValue (fromInteger x) W
  negate (PowerValue x u) = PowerValue (negate x) u

instance Fractional (PowerValue a b) where
  fromRational x = PowerValue (fromRational x) W
  pv1 / pv2 = convert siFrac destUnit
    where
      siFrac = toWatts $ siValue pv1 / siValue pv2
      destUnit = largerUnit pv1 pv2

instance Ord (PowerValue a b) where
 compare pv1 pv2 = compare siValue1 siValue2
  where
    siValue1 = value $ toSi pv1
    siValue2 = value $ toSi pv2 

instance Real (PowerValue a b) where
  toRational powerValue = toRational siValue
    where siValue = value $ toSi powerValue
