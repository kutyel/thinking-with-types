{-# language DataKinds      #-}
{-# language PolyKinds      #-}
{-# language TypeFamilies   #-}
{-# language TypeOperators  #-}

module Chapter2 where

import GHC.TypeLits

import Control.Monad.Trans.Class (MonadTrans)
import Data.Proxy                (Proxy)

-- Exercise 2.1.3-i
-- *Chapter2> :k Show
-- Show :: Type -> Constraint

-- Exercise 2.1.3-ii
-- *Chapter2> :k Functor
-- Functor :: (Type -> Type) -> Constraint

-- Exercise 2.1.3-iii
-- *Chapter2> :k Monad
-- Monad :: (Type -> Type) -> Constraint

-- Exercise 2.1.3-iv
-- *Chapter2> :k MonadTrans
-- MonadTrans :: ((Type -> Type) -> Type -> Type) -> Constraint

-- `-XDataKinds` lifts data constructors into
-- *type constructors* and types into *kinds*.

-- *Chapter2> :set -XDataKinds
-- *Chapter2> :k "hello"
-- "hello" :: Symbol

-- *Chapter2> :k AppendSymbol
-- AppendSymbol :: Symbol -> Symbol -> Symbol

-- *Chapter2> :k CmpSymbol
-- CmpSymbol :: Symbol -> Symbol -> Ordering

-- *Chapter2> :k 1234
-- 1234 :: Nat

-- *Chapter2> :set -XTypeOperators
-- *Chapter2> :kind! 4 + 3
-- 4 + 3 :: Nat
-- = 7

-- *Chapter2> :k '[]
-- '[] :: [a]

-- *Chapter2> :k '(:)
-- '(:) :: a -> [a] -> [a]

-- *Chapter2> :k [Bool]
-- [Bool] :: Type
-- *Chapter2> :k '[Bool]
-- '[Bool] :: [Type]
-- *Chapter2> :k '[ 'True]
-- '[ 'True] :: [Bool]

-- Tuples
-- *Chapter2> :k '(1, "flavio")
-- '(1, "flavio") :: (Nat, Symbol)

type family Or (x :: Bool) (y :: Bool) :: Bool where
  Or 'True  y = 'True
  Or 'False y = y

-- Exercise 2.4-i
-- Write a closed type family to compute Not.
-- (for reference, :t not -> not :: Bool -> Bool)
type family Not (x :: Bool) :: Bool where
  Not 'True = 'False
  Not _     = 'True

-- *Chapter2> :kind! Not 'False
-- Not 'False :: Bool
-- = 'True
-- It works! 🎉

type family Map (x :: a -> b) (i :: [a]) :: [b] where
  Map f '[]       = '[]
  Map f (x ': xs) = f x ': Map f xs

-- since there is no currying at the type-level, this isn't very useful...
-- FIXME: :t undefined :: Proxy (Map (Or 'True) '[ 'True, 'False, 'False ])

-- > prepare for **first class families**! (chapter 10)

-- *Chapter2> :k Or
-- Or :: Bool -> Bool -> Bool
-- the *last* :: Bool is the RETURN type! ⚠️

type family Foo (x :: Bool) (y :: Bool) :: Bool

-- *Chapter2> :k Foo
-- Foo :: Bool -> Bool -> Bool

type family Bar x y :: Bool -> Bool -> Bool

-- *Chapter2> :k Bar
-- Bar :: Type -> Type -> Bool -> Bool -> Bool

-- think of closed type families as type-level functions (for now).

-- EXTRA EXTRA!! 🗞🗞🗞
{- | Check that a type is an element of a list (of types):
>>> :kind! Elem String '[]
Elem String '[] :: Bool
= 'False
>>> :kind! Elem Bool '[Int, Bool]
Elem Bool '[Int, Bool] :: Bool
= 'True
>>> :kind! Elem String '[Int, Bool]
Elem String '[Int, Bool] :: Bool
= 'False
-}
type family Elem (e :: t) (es :: [t]) :: Bool where
  Elem _ '[]       = 'False
  Elem x (x ': xs) = 'True
  Elem x (_ ': xs) = Elem x xs
