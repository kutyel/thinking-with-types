{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Chapter14 where

-- Indexed Monads

import Control.Monad.Indexed
import Data.Coerce
import Fcf
import GHC.TypeLits (Nat)
import qualified GHC.TypeLits as TL
import Language.Haskell.DoNotation
import qualified System.IO as SIO
import System.IO hiding (Handle, openFile)
import Prelude hiding (Monad (..))

-- class IxApplicative m => IxMonad m where
--   ibind :: (a -> m j k b) -> m i j a -> m i k b

newtype Ix m i j a
  = Ix
      { unsafeRunIx :: m a
      }
  deriving (Functor, Applicative, Monad)

instance Functor m => IxFunctor (Ix m) where
  imap = fmap

instance Applicative m => IxPointed (Ix m) where
  ireturn = Prelude.pure

instance Applicative m => IxApplicative (Ix m) where
  iap ::
    forall i j k a b.
    Ix m i j (a -> b) ->
    Ix m j k a ->
    Ix m i k b
  iap = coerce $ (<*>) @m @a @b

instance Monad m => IxMonad (Ix m) where
  ibind ::
    forall i j k a b.
    (a -> Ix m j k b) ->
    Ix m i j a ->
    Ix m i k b
  ibind = coerce $ (=<<) @m @a @b

-- Linear Allocations
data LinearState
  = LinearState
      { linearNextKey :: Nat,
        linearOpenKeys :: [Nat]
      }

newtype Linear s (i :: LinearState) (j :: LinearState) a
  = Linear
      { unsafeRunLinear :: Ix IO i j a
      }
  deriving (IxFunctor, IxPointed, IxApplicative, IxMonad)

newtype Handle s key
  = Handle
      { unsafeGetHandle :: SIO.Handle
      }

openFile ::
  FilePath ->
  IOMode ->
  Linear s ('LinearState next open)
    ('LinearState (next TL.+ 1) (next ': open))
    (Handle s next)
openFile = coerce SIO.openFile

type IsOpen (key :: k) (ts :: [k]) =
  IsJust =<< Find (TyEq key) ts

type Close (key :: k) (ts :: [k]) =
  Filter (Not <=< TyEq key) ts

closeFile ::
  Eval (IsOpen key open) ~ 'True =>
  Handle s key ->
  Linear s ('LinearState next open)
    ('LinearState next (Eval (Close key open)))
    ()
closeFile = coerce SIO.hClose

runLinear ::
  ( forall s.
    Linear s ('LinearState 0 '[])
      ('LinearState n '[])
      a
  ) ->
  IO a
runLinear = coerce
