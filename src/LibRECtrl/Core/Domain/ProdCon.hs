module LibRECtrl.Core.Domain.ProdCon
  ( ProdCon (..),
    isProduction,
    isConsumption,
    isBalance,
    rawValue
  )
where

-- | The ProdCon type represents values of three categories.
-- A value of type @'ProdCon'@ a is either a @'Production'@ (a source),
-- a @'Consumption'@ (sink) or a @'Balance'@ between a @'Production'@ and a @'Consumption'@ (e.g., a surplus or a deficit).
--
-- Any chain of actions via the 'Applicative' or 'Monad' instance will result in a 'Balance'.
data ProdCon a
  = Production a
  | Consumption a
  | Balance a
  deriving
    ( Eq,
      Ord,
      Show,
      Read
    )

-- | Convenience function for determining if a 'ProdCon' is a 'Production'.
isProduction :: ProdCon a -> Bool
isProduction (Production _) = True
isProduction _ = False

-- | Convenience function for determining if a 'ProdCon' is a 'Consumption'.
isConsumption :: ProdCon a -> Bool
isConsumption (Consumption _) = True
isConsumption _ = False

-- | Convenience function for determining if a 'ProdCon' is a 'Balance'.
isBalance :: ProdCon a -> Bool
isBalance (Balance _) = True
isBalance _ = False

-- | Extracts the raw value from a 'ProdCon'
--
-- ==== __Example__
--
-- >>> rawValue (Production 1)
-- 1
rawValue :: ProdCon a -> a
rawValue (Production x) = x
rawValue (Consumption x) = x
rawValue (Balance x) = x

-- | Similar to a Functor, but used by the 'Applicative' instance to convert 'Production' and 'Consumption' types into 'Balance' tpes.
class Balancer f where
  balance :: (a -> b) -> f a -> f b

instance Balancer ProdCon where
  balance f (Production a) = Balance (f a)
  balance f (Consumption a) = Balance (f a)
  balance f (Balance a) = Balance (f a)

instance Functor ProdCon where
  fmap f (Production a) = Production (f a)
  fmap f (Consumption a) = Consumption (f a)
  fmap f (Balance a) = Balance (f a)

instance Applicative ProdCon where
  pure = Balance
  Production f <*> m = balance f m
  Consumption f <*> m = balance f m
  Balance f <*> m = balance f m

instance Monad ProdCon where
  Production r >>= k = k r
  Consumption r >>= k = k r
  Balance r >>= k = k r

