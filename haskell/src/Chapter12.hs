{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Chapter12 where

import Data.Kind (Type)
import Fcf
import GHC.TypeLits

instance
  ( TypeError
      ( Text "Attempting to interpret a number as a function "
          :$$: Text "in the type `" :<>: ShowType (a -> b) :<>: Text "`"
          :$$: Text "Did you forget to specify the function you wanted?"
      )
  ) =>
  Num (a -> b)

-- exercise 12-i
type family FriendlyFindElem (f :: Symbol) (key :: Symbol) (ts :: [(Symbol, k)]) where
  FriendlyFindElem f key ts =
    Eval
      ( FromMaybe
          ( TypeError
              ( 'Text "Attempted to call `" ':<>: 'Text f ':<>: 'Text "` with key `" ':<>: 'Text key ':<>: 'Text "`."
                  ':$$: 'Text "But the OpenProduct only has keys:"
                  ':$$: 'Text " " ':<>: ShowList (Eval (Map Fst ts))
              )
          )
          =<< FindIndex (TyEq key <=< Fst) ts
      )

-- exercise 12-ii
type family ShowList (ts :: [k]) where
  ShowList '[] = Text ""
  ShowList (a ': '[]) = ShowType a
  ShowList (a ': as) = ShowType a ':<>: Text ", " ':<>: ShowList as
-- exercise 12-iii
-- it automatically sees the error and thus is thrown
-- foo :: TypeError ('Text "error msg") => a
