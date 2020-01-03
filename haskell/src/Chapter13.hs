{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Chapter13 where

import GHC.Generics

-- let's write a generic Eq instance!
class GEq a where
  geq :: a x -> a x -> Bool

instance GEq U1 where -- () -- empty data constructor
  geq U1 U1 = True

instance GEq V1 where -- Void
  geq _ _ = True

instance Eq a => GEq (K1 _1 a) where -- type inside a data constructor
  geq (K1 a) (K1 b) = a == b

instance (GEq a, GEq b) => GEq (a :+: b) where -- sum types!
  geq (L1 a1) (L1 a2) = geq a1 a2
  geq (R1 b1) (R1 b2) = geq b1 b2
  geq _ _ = False

instance (GEq a, GEq b) => GEq (a :*: b) where -- product types!
  geq (a1 :*: b1) (a2 :*: b2) = geq a1 a2 && geq b1 b2

-- D1, C1 and S1 (all types of metadata) are synonyms of M1
instance GEq a => GEq (M1 _x _y a) where -- metadata
  geq (M1 a1) (M1 a2) = geq a1 a2

genericEq :: (Generic a, GEq (Rep a)) => a -> a -> Bool
genericEq a b = geq (from a) (from b)

data Foo a b c
  = F0
  | F1 a
  | F2 b c
  deriving (Generic)

instance (Eq a, Eq b, Eq c) => Eq (Foo a b c) where
  (==) = genericEq

-- TODO: exercise 13.2-i
-- provide a generic instance for the Ord class.
class GOrd a where
  gcompare :: a x -> a x -> Ordering

-- TODO: exercise 13.2-ii
-- implement the function exNihilo :: Maybe a with GHC.Generics
-- Just a if a is one data constructor which takes zero arguments, Nothing otherwise
class GexNihilo a where
  gexNihilo :: Maybe (a x)
