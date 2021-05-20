module LibRECtrl.Core.Domain.Power
  ( PowerUnit (..),
    Power (..),
    PCPower (..),
    producerSurplus,
    producerDeficit
  )
where

import LibRECtrl.Core.Domain.ProdCon
import LibRECtrl.Core.Domain.Unit
import Numeric

-- | The power units supported by this library.
data PowerUnit
  = W
  | KW
  | MW
  | GW
  | UserDefinedPowerUnit
      { -- | The PowerUnit on which this user defined unit is based
        baseUnit :: PowerUnit,
        -- | The conversion factor, used to convert this unit into its baseUnit
        conversionFactor :: Double,
        -- | The name of this unit
        unitName :: String
      }
  deriving (Eq)

-- | A Power is a @'LibRECtrl.Core.Domain.Unit.PhysicalValue'@ that represents power.
-- It comes with a Double value and a PowrUnit.
data Power = Power
  { -- | The numeric value.
    value :: Double,
    -- | The power unit.
    unit :: PowerUnit
  }

-- | Producer/Consumer/Balance power.
type PCPower = ProdCon Power

-- | Computes the power surplus between production and consumption.
-- 'source': A 'Production' instance.
-- 'sink': A 'Consumption' instance.
-- returns a 'Balance' instance with a positive 'Power' surplus, or 0 if there is no surplus.
producerSurplus :: PCPower -> PCPower -> PCPower
producerSurplus source sink = positiveDiff <$> source <*> sink

-- | Computes the power deficit between production and consumption.
-- 'source': A 'Production' instance.
-- 'sink': A 'Consumption' instance.
-- returns a 'Balance' instance with a negative 'Power' deficit, or 0 if there is no surplus.
producerDeficit :: PCPower -> PCPower -> PCPower
producerDeficit source sink = negativeDiff <$> source <*> sink

positiveDiff :: Power -> Power -> Power
positiveDiff x y = max 0 $ x - y

negativeDiff :: Power -> Power -> Power
negativeDiff x y = min 0 $ x - y

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

instance Show Power where
  show (Power x u) = mconcat [showFloat x "", " ", show u]

instance PhysicalValue Power where
  toSi (Power x u) =
    Power
      { value = siValue,
        unit = si u
      }
    where
      factor = siFactor u
      summand = siOffset u
      siValue = factor * x + summand

-- | Extract the SI value from a Power
siValue :: Power -> Double
siValue = value . toSi

-- | Determine the larger unit of two Powers
largerUnit :: Power -> Power -> PowerUnit
largerUnit pv1 pv2 = max (unit pv1) (unit pv2)

-- | Convert a PhysicalValue with a given unit to an equivalent PhysicalValue with another unit
convert :: Power -> PowerUnit -> Power
convert powerValue destUnit = Power destValue destUnit
  where
    destValue = siValue powerValue / siFactor destUnit

-- | Factory to create a Power in Watts from a Double value
toWatts :: Double -> Power
toWatts x = Power x W

instance Eq Power where
  pv1 == pv2 = siValue pv1 == siValue pv2

instance Num Power where
  pv1 + pv2 = convert siSum destUnit
    where
      siSum = toWatts $ siValue pv1 + siValue pv2
      destUnit = largerUnit pv1 pv2
  pv1 * pv2 = convert siProduct destUnit
    where
      siProduct = toWatts $ siValue pv1 * siValue pv2
      destUnit = largerUnit pv1 pv2
  abs (Power x u) = Power (abs x) u
  signum (Power x u) = Power (signum x) u
  fromInteger x = Power (fromInteger x) W
  negate (Power x u) = Power (negate x) u

instance Fractional Power where
  fromRational x = Power (fromRational x) W
  pv1 / pv2 = convert siFrac destUnit
    where
      siFrac = toWatts $ siValue pv1 / siValue pv2
      destUnit = largerUnit pv1 pv2

instance Ord Power where
  compare pv1 pv2 = compare siValue1 siValue2
    where
      siValue1 = value $ toSi pv1
      siValue2 = value $ toSi pv2

instance Real Power where
  toRational powerValue = toRational siValue
    where
      siValue = value $ toSi powerValue
