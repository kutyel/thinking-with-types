{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Chapter10 where

import Data.Kind (Constraint, Type)

newtype Fst a b = Fst (a, b)

class Eval' l t | l -> t where
  eval :: l -> t

instance Eval' (Fst a b) a where
  eval (Fst (a, b)) = a

-- exercise 10.1-i) defunctionalize listToMaybe :: [a] -> Maybe a
newtype ListToMaybe' a = ListToMaybe' [a]

instance Eval' (ListToMaybe' a) (Maybe a) where
  eval (ListToMaybe' []) = Nothing
  eval (ListToMaybe' [x]) = Just x

data MapList' dfb a = MapList' (a -> dfb) [a]

instance Eval' dfb dft => Eval' (MapList' dfb a) [dft] where
  eval (MapList' f []) = []
  eval (MapList' f (a : as)) = eval (f a) : eval (MapList' f as)

-- type-level defunctionalization
type Exp a = a -> Type

type family Eval (e :: Exp a) :: a

data Snd :: (a, b) -> Exp b

type instance Eval (Snd '(a, b)) = b

data FromMaybe :: a -> Maybe a -> Exp a

type instance Eval (FromMaybe _ ('Just a)) = a

type instance Eval (FromMaybe a 'Nothing) = a

-- exercise 10.2-i) defunctionalize listToMaybe at the type-level.
data ListToMaybe :: [a] -> Exp (Maybe a)

type instance Eval (ListToMaybe '[]) = 'Nothing

type instance Eval (ListToMaybe (a ': _)) = 'Just a

-- higher-order functions with type families!

data MapList :: (a -> Exp b) -> [a] -> Exp [b]

type instance Eval (MapList f '[]) = '[]

type instance Eval (MapList f (a ': as)) = Eval (f a) ': Eval (MapList f as)

-- exercise 10.2-ii) defunctionalize foldr :: (a -> b -> b) -> b -> [a] -> b
data Foldr :: (a -> b -> Exp b) -> b -> [a] -> Exp b

type instance Eval (Foldr _ z '[]) = z

type instance Eval (Foldr f z (x ': xs)) = Eval (Foldr f (Eval (f x z)) xs)

-- working with first class families
data Pure :: a -> Exp a

type instance Eval (Pure x) = x

data (=<<) :: (a -> Exp b) -> Exp a -> Exp b

type instance Eval (k =<< e) = Eval (k (Eval e))

infixr 0 =<<

data (<=<) :: (b -> Exp c) -> (a -> Exp b) -> a -> Exp c

type instance Eval ((f <=< g) x) = Eval (f (Eval (g x)))

infixr 1 <=< -- function composition at the type-level!! 🤯

data TyEq :: a -> b -> Exp Bool

type instance Eval (TyEq a b) = TyEqImpl a b

type family TyEqImpl (a :: k) (b :: k) :: Bool where
  TyEqImpl a a = 'True
  TyEqImpl a b = 'False

data Collapse :: [Constraint] -> Exp Constraint

type instance Eval (Collapse '[]) = (() :: Constraint)

type instance Eval (Collapse (a ': as)) = (a, Eval (Collapse as))

type All (c :: k -> Constraint) (ts :: [k]) = Collapse =<< MapList (Pure1 c) ts

data Pure1 :: (a -> b) -> a -> Exp b

type instance Eval (Pure1 f x) = f x

-- ad-hoc polymorphism
data Map :: (a -> Exp b) -> f a -> Exp (f b)

type instance Eval (Map f '[]) = '[]

type instance Eval (Map f (a ': as)) = Eval (f a) ': Eval (Map f as)

type instance Eval (Map f 'Nothing) = 'Nothing

type instance Eval (Map f ('Just a)) = 'Just (Eval (f a))

type instance Eval (Map f ('Left x)) = 'Left x

type instance Eval (Map f ('Right a)) = 'Right (Eval (f a))

-- exercise 10.4-i) promoted functor instance for tuples
type instance Eval (Map f '(a, b)) = '(a, Eval (f b))

data Mappend :: a -> a -> Exp a

type instance Eval (Mappend '() '()) = '()

type instance Eval (Mappend (a :: Constraint) (b :: Constraint)) = (a, b)

-- type instance Eval (Mappend (a :: [k]) (b :: [k])) = Eval (a ++ b) -- error? 🤔

data Mempty :: k -> Exp k

type instance Eval (Mempty '()) = '()

type instance Eval (Mempty (c :: Constraint)) = (() :: Constraint)

type instance Eval (Mempty (l :: [k])) = '[]
