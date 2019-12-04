{-# language ConstraintKinds      #-}
{-# language DataKinds            #-}
{-# language FlexibleInstances    #-}
{-# language GADTs                #-}
{-# language ScopedTypeVariables  #-}
{-# language TypeFamilies         #-}
{-# language TypeOperators        #-}
{-# language UndecidableInstances #-}

module Chapter5 where

import           Data.Kind (Constraint, Type)

-- my first GADT 😇
data HList (ts :: [Type]) where
  HNil :: HList '[]
  (:#) :: t -> HList ts -> HList (t ': ts)
infixr 5 :#

hLength :: HList ts -> Int
hLength HNil      = 0
hLength (_ :# ts) = 1 + hLength ts

hHead :: HList (t ': ts) -> t
hHead (t :# _) = t

-- Eq

-- instance Eq (HList '[]) where
--   HNil == HNil = True

-- instance (Eq t, Eq (HList ts)) => Eq (HList (t ': ts)) where
--   (x :# xs) == (y :# ys) = x == y && xs == ys

-- exercise 5.3-i

-- instance Ord (HList '[]) where
--   compare HNil HNil = EQ

-- instance (Ord t, Ord (HList ts)) => Ord (HList (t ': ts)) where
--   compare (x :# xs) (y :# ys) = compare x y <> compare xs ys

-- exercise 5.3-ii

-- instance Show (HList '[]) where
--   show HNil = "HNil"

-- instance (Show t, Show (HList ts)) => Show (HList (t ': ts)) where
--   show (x :# xs) = show x ++ " :# " ++ show xs

-- First useful type family!

type family All (c :: Type -> Constraint) (ts :: [Type]) :: Constraint where
  All c '[]       = ()
  All c (t ': ts) = (c t, All c ts)

-- using All to simplify instances!

instance All Eq ts => Eq (HList ts) where
  HNil == HNil           = True
  (x :# xs) == (y :# ys) = x == y && xs == ys

-- exercise 5.3-iii

instance (All Eq ts, All Ord ts) => Ord (HList ts) where
  compare HNil HNil           = EQ
  compare (x :# xs) (y :# ys) = compare x y <> compare xs ys

instance All Show ts => Show (HList ts) where
  show HNil      = "HNil"
  show (x :# xs) = show x ++ " :# " ++ show xs
