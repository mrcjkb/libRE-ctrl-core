module LibRECtrl.Core.Domain.Unit where

-- | A physical unit, which has an SI Unit that any type instance can be converted to via the siFactor and siSummand functions
class (Eq a, Ord a, Show a) => Unit a where
  -- | Get the SI Unit type
  si :: a -> a
  -- | The conversion factor, used to convert a value of this Unit to its SI unit via multiplication
  siFactor :: a -> Double
  -- | The conversion offset, used to convert a value of this Unit to its SI unit via addition
  siOffset :: a -> Double

-- | Implementation of the Ord compare function for Units
compare' :: Unit a => a -> a -> Ordering
compare' unit1 unit2 = compare (unitScale unit1) (unitScale unit2)
  where unitScale unit = (siFactor unit) + (siOffset unit)

-- | A physical value, which has a unit, and can be converted to its SI value.
-- | A PhysicalValue supports addition and subtration with another PhysicalValue,
-- | and it supports multiplication and division with a scalar. A scalar should be divided by a PhysicalValue.
-- | Note that these rules are currently not enforced!
class (Eq a, Real a, Fractional a, Show a) => PhysicalValue a where
  -- | Convert a PhysicalValue with a given unit to an equivalent PhysicalValue with its SI unit
  toSi :: a -> a
