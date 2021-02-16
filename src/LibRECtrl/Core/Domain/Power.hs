{-#LANGUAGE GADTs#-}
module LibRECtrl.Core.Domain.Power(
      PowerUnit(..),
      PowerValue(..)
) where

import LibRECtrl.Core.Domain.Unit
import Numeric

-- | The power units supported by this library.
data PowerUnit = W | KW | MW | GW deriving Eq

instance Show PowerUnit where
  show W = "W"
  show KW = "kW"
  show MW = "MW"
  show GW = "GW"

instance Unit PowerUnit where
  si _ = W
  siFactor W = 1
  siFactor KW = 1000
  siFactor MW = 1000 * siFactor KW
  siFactor GW = 1000 * siFactor MW
  siSummand _ = 0

-- | A PowerValue has a value and a unit.
data PowerValue a where
  PowerValue :: {
    value :: Double,
    unit :: PowerUnit
  } -> PowerValue a

instance Show (PowerValue a) where
  show (PowerValue x u) = mconcat [showFloat x "", " ", show u]

instance PhysicalValue (PowerValue a) where
  toSi (PowerValue x u) = PowerValue {
                            value = siValue,
                            unit = si u
                          }
    where
      factor = siFactor u
      summand = siSummand u
      siValue = factor * x + summand

instance Eq (PowerValue a) where
  pv1 == pv2 = siValue1 == siValue2
    where
      siValue1 = value $ toSi pv1
      siValue2 = value $ toSi pv2
