module LibRECtrl.Core.Domain.Unit where

-- | A physical unit, which has an SI Unit that any type instance can be converted to via the siFactor and siSummand functions
class (Eq a) => Unit a where
  -- | Get the SI Unit type
  si :: a -> a
  -- | The conversion factor, used to convert a value of this Unit to its SI unit via multiplication
  siFactor :: Num b =>  a -> b
  -- | The conversion offset, used to convert a value of this Unit to its SI unit via addition
  siOffset :: Num b => a -> b
 

-- | A physical value, which has a unit, and can be converted to its SI value.
class (Eq c) => PhysicalValue c where
  -- | Convert a PhysicalValue with a given unit to an equivalent PhysicalValue with its SI unit
  toSi :: c -> c
