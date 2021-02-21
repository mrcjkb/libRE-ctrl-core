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
   compare unit1 unit2 = compareUnits unit1 unit2 

instance Unit PowerUnit where
  si _ = W
  siFactor W = 1
  siFactor KW = 1000
  siFactor MW = 1000 * siFactor KW
  siFactor GW = 1000 * siFactor MW
  siFactor (UserDefinedPowerUnit baseUnit x _) = x * siFactor baseUnit
  siOffset _ = 0

-- | A PowerValue has a value and a unit.
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

-- | Convert a PhysicalValue with a given unit to an equivalent PhysicalValue with another unit
convert :: PowerValue a b -> PowerUnit -> PowerValue a b
convert powerValue destUnit = PowerValue destValue destUnit
  where
    siValue = value $ toSi powerValue
    destValue = siValue / siFactor destUnit
    
instance Eq (PowerValue a b) where
  pv1 == pv2 = siValue1 == siValue2
    where
      siValue1 = value $ toSi pv1
      siValue2 = value $ toSi pv2

--instance Num (PowerValue a) where
  --(+)  

instance Ord (PowerValue a b) where
 compare pv1 pv2 = compare siValue1 siValue2
  where
    siValue1 = value $ toSi pv1
    siValue2 = value $ toSi pv2 

--instance Real (PowerValue a b) where
  --toRational powerValue = toRational siValue
    --where siValue = value $ toSi powerValue
