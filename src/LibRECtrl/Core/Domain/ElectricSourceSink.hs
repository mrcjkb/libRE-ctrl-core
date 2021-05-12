module LibRECtrl.Core.Domain.ElectricSourceSink
   (
    ElectricSource
   ) where

-- | A container for an electric source that comes from a power source (power production).
data ElectricSource a
  = -- | Comes from a photovoltaic source.
    PV a
  | -- | Comes from an undefined source.
    UndefinedSource a
  deriving (Eq, Show, Ord, Read)

-- | A container for an electric @'PowerValue'@ that is consumed (as a power sink).
data ElectricSink a
  = --  | Represents a sum of various electrical sinks.
    Multiple a
  | --  | Represents a heat pump load.
    HeatPump a
  | --  | Represents a remainder of electrical load.
    --    For example, a @Multiple@ could consist of a @'HeatPump'@ and a @'Remaining'@.
    Remaining a
  | -- | Represents an electrical load from an undefined consumer.
    UndefinedSink a
  deriving (Eq, Show, Ord, Read)

-- | Container for a power source/sink difference.
data SourceSinkDifference a
  = -- | An absolute surplus that remains from an @'ElectricSource'@ after consumption by an @'ElectricSink'@'.
    SourceSurplus a
  | -- | An absolute deficit that remains if a demand cannot be met by an @'ElectricSource'@.
    SourceDeficit a
  deriving (Eq, Show, Ord, Read)

instance Functor ElectricSource where
    fmap f (PV a) = PV (f a)
    fmap f (UndefinedSource a) = UndefinedSource (f a)

instance Functor ElectricSink where
    fmap f (Multiple a) = Multiple (f a)
    fmap f (HeatPump a) = HeatPump (f a)
    fmap f (Remaining a) = Remaining (f a)
    fmap f (UndefinedSink a) = UndefinedSink (f a)

-- powerSurplus :: ElectricSourcePower -> ElectricSinkPower -> SourceSinkDifferencePower
-- powerSurplus (PVProduction source) (TotalElectricalLoad sink) = powerSurplus' source sink
-- -- TODO - Rough draft as note for later implemenmtation
-- -- TODO: Implement Monad for SourceSinkDifferencePower, ElectricSourcePower and ElectricSinkPower

-- -- | Helper function for computing the power surplus from two power values
-- powerSurplus' :: PowerValue -> PowerValue -> SourceSinkDifferencePower
-- powerSurplus' source sink = PowerSurplus $ max 0 source - sink
