{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}

module Chapter15Singletons where

import Data.Aeson
import Data.Constraint
import Data.Kind (Type)
import Data.Maybe (mapMaybe)
import Data.Singletons.Prelude
import Data.Singletons.TH
import Unsafe.Coerce (unsafeCoerce)

-- singletons [d|
--   data TimeOfDay
--     = Morning
--     | Afternoon
--     | Evening
--     | Night
--     deriving (Eq, Ord, Show)
--   |]

-- data Decision a
--   = Proved a
--   | Disproved (a -> Void)

instance (Eq (Demote k), SingKind k)
    => SDecide k where
  a %~ b =
    if fromSing a == fromSing b
      then Proved $ unsafeCoerce Refl
      else Disproved $ const undefined

-- instance SDecide Bool where
--   STrue %~ STrue = Proved Refl
--   SFalse %~ SFalse = Proved Refl
--   _ %~ _ = Disproved $ const undefined

-- exercise 15.4-i
-- instance SDecide a => SDecide (Maybe a) where
--   SNothing %~ SNothing = Proved Refl
--   SJust a %~ SJust b =
--     case a %~ b of
--       Proved Refl -> Proved Refl
--       Disproved _ -> Disproved $ const undefined

data Sigma (f :: k -> Type) where
  Sigma :: Sing a -> f a -> Sigma f

withSigma
  :: (forall (a :: k). Sing a -> f a -> r)
  -> Sigma f
  -> r
withSigma c (Sigma s f) = c s f

toSigma :: SingI a => f a -> Sigma f
toSigma = Sigma sing

fromSigma
  :: forall k (a :: k) (f :: k -> Type).
    (SingI a, SDecide k)
  => Sigma f
  -> Maybe (f a)
fromSigma (Sigma s f) =
  case s %~ sing @a of
    Proved Refl -> Just f
    Disproved _ -> Nothing

class Dict1 c (f :: k -> Type) where
  dict1 :: Sing (a :: k) -> Dict (c (f a))

instance (Dict1 Eq (f :: k -> Type), SDecide k)
    => Eq (Sigma f) where
  Sigma sa fa == Sigma sb fb =
    case sa %~ sb of
      Proved Refl ->
        case dict1 @Eq @f sa of
          Dict -> fa == fb
      Disproved _ -> False

instance ( Dict1 Show (f :: k -> Type)
         , Show (Demote k)
         , SingKind k
         ) => Show (Sigma f) where
  show (Sigma sa fa) =
    case dict1 @Show @f sa of
      Dict -> mconcat [ "Sigma "
          , show $ fromSing sa
          , " ("
          , show fa
          , ")"
        ]

-- exercise 15.5-i
instance ( Dict1 Eq (f :: k -> Type)
         , Dict1 Ord f
         , SDecide k
         , SingKind k
         , Ord (Demote k)
         ) => Ord (Sigma f) where
  Sigma sa fa `compare` Sigma sb fb =
    case sa %~ sb of
      Proved Refl ->
        case dict1 @Ord @f sa of
          Dict -> fa `compare` fb
      Disproved _ ->
        fromSing sa `compare` fromSing sb
