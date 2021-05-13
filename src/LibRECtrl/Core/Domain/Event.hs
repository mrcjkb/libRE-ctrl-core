module LibRECtrl.Core.Domain.Event
  ( TimeStamped,
    UTCTimeStampedEvent,
  )
where

import Data.Time.Clock

-- | A value that a time stamp
class TimeStamped a where
  -- | Returns the difference in time between two TimeStamped instances
  diffTime :: a -> a -> NominalDiffTime

-- | An event with a UTC time stamp
data UTCTimeStampedEvent event = UTCTimeStampedEvent UTCTime event deriving (Eq, Show)

instance (TimeStamped a) => TimeStamped (UTCTimeStampedEvent a) where
  diffTime (UTCTimeStampedEvent t1 _) (UTCTimeStampedEvent t2 _) = diffUTCTime t1 t2

instance (Ord a) => Ord (UTCTimeStampedEvent a) where
  compare (UTCTimeStampedEvent t1 _) (UTCTimeStampedEvent t2 _) = compare t1 t2

instance Functor UTCTimeStampedEvent where
  fmap f (UTCTimeStampedEvent t x) = UTCTimeStampedEvent t $ f x

