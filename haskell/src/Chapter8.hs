{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}

module Chapter8 where

import Data.Coerce (coerce)
import qualified Data.Map as M
import Data.Monoid (Product (..), Sum (..))

slowSum :: [Int] -> Int
slowSum = getSum . mconcat . fmap Sum

fastSum :: [Int] -> Int
fastSum = getSum . mconcat . coerce

newtype Reverse a
  = Reverse
      { getReverse :: a
      }
  deriving (Eq, Show)

instance Ord a => Ord (Reverse a) where
  compare (Reverse a) (Reverse b) = compare b a

foo :: M.Map Char (Reverse Bool)
foo = coerce (M.singleton 'S' True)

-- baz :: M.Map (Reverse Char) Bool
-- baz = coerce (M.singleton 'S' True) ❌ errors out (keys are `nominal`)

-- exercise 8.2-i) what is the role of Either a b?
-- type role Either representational representational

--exercise 8.2-ii) Proxy a?
-- type role Proxy phantom

type family IntToBool a where
  IntToBool Int = Bool
  IntToBool a = a

data BST v
  = Empty
  | Branch (BST v) v (BST v)

type role BST nominal
-- you can always "strengthen" inferred roles, but never weaken them.
