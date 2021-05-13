module LibRECtrl.Core.Domain.Power
  ( PowerUnit (..),
    Power (..),
    PowerType,
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

data ElectricPower
  = PV (ProdCon Power)
  | ConsumerLoad (ProdCon Power)
  | PowerBalance (ProdCon Power)

pvPower :: Power -> ElectricPower
pvPower p = PV $ Producer p

consumerLoad :: Power -> ElectricPower
consumerLoad p = ConsumerLoad $ Consumer p

powerBalance :: ProdCon Power -> ProdCon Power -> ProdCon Power
powerBalance (Producer p) (Consumer c) = powerBalance' (Producer p) (Consumer c)
powerBalance (Consumer c) (Producer p) = powerBalance' (Producer p) (Consumer c)
powerBalance (Producer p1) (Producer p2) = Producer $ p1 + p2
powerBalance (Consumer c1) (Consumer c2) = Consumer $ c1 + c2

powerBalance' :: ProdCon Power -> ProdCon Power -> ProdCon Power
powerBalance' (Producer producer) (Consumer consumer) =
  if surplus >= 0
    then Producer $ positiveDiff producer consumer
    else Consumer $ positiveDiff consumer producer
  where
    surplus :: Power
    surplus = producer - consumer
    positiveDiff :: Power -> Power -> Power
    positiveDiff production consumption = max 0 $ production - consumption

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
