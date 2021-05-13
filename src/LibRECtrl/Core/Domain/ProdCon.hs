module LibRECtrl.Core.Domain.ProdCon
  ( ProdCon (..),
  )
where

data ProdCon a = Producer a | Consumer a
  deriving
    ( Eq,
      Ord,
      Show,
      Read
    )

instance Functor ProdCon where
  fmap f (Producer a) = Producer (f a)
  fmap f (Consumer a) = Consumer (f a)
