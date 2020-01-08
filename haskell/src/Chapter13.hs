{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -O -fplugin Test.Inspection.Plugin #-}

module Chapter13 where

import Control.Monad.Writer
import Data.Aeson ((.=), ToJSON, Value (..), object)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.Kind (Type)
import Data.Text (Text, pack)
import Data.Typeable
import Data.Vector (fromList)
import GHC.Generics
import GHC.TypeLits
import qualified GHC.TypeLits as Err
import Test.Inspection

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
  deriving (Generic, MyEq) -- XDeriveAnyClass

instance (Eq a, Eq b, Eq c) => Eq (Foo a b c) where
  (==) = genericEq

instance (Ord a, Ord b, Ord c) => Ord (Foo a b c) where
  compare = genericOrd

-- exercise 13.2-i
-- provide a generic instance for the Ord class.
class GOrd a where
  gord :: a x -> a x -> Ordering

instance GOrd U1 where -- () -- empty data constructor
  gord _ _ = EQ

instance GOrd V1 where -- Void
  gord _ _ = EQ

instance Ord a => GOrd (K1 _1 a) where -- type inside a data constructor
  gord (K1 a) (K1 b) = compare a b

instance (GOrd a, GOrd b) => GOrd (a :+: b) where -- sum types!
  gord (L1 a1) (L1 a2) = gord a1 a2
  gord (R1 b1) (R1 b2) = gord b1 b2
  gord (L1 _) (R1 _) = LT
  gord (R1 _) (L1 _) = GT

instance (GOrd a, GOrd b) => GOrd (a :*: b) where -- product types!
  gord (a1 :*: b1) (a2 :*: b2) = gord a1 a2 <> gord b1 b2

instance GOrd a => GOrd (M1 _x _y a) where -- metadata
  gord (M1 a1) (M1 a2) = gord a1 a2

genericOrd :: (Generic a, GOrd (Rep a)) => a -> a -> Ordering
genericOrd a b = gord (from a) (from b)

-- exercise 13.2-ii
-- implement the function exNihilo :: Maybe a with GHC.Generics
-- `Just a` if a is one data constructor which takes zero arguments, Nothing otherwise
class GexNihilo a where
  gexNihilo :: Maybe (a x)

instance GexNihilo U1 where
  gexNihilo = Just U1

instance GexNihilo V1 where
  gexNihilo = Nothing

instance GexNihilo (K1 _x a) where
  gexNihilo = Nothing

instance GexNihilo (a :+: b) where
  gexNihilo = Nothing

instance GexNihilo (a :*: b) where
  gexNihilo = Nothing

instance GexNihilo a => GexNihilo (M1 _x _y a) where -- metadata
  gexNihilo = fmap M1 gexNihilo

-- typeclasses we defined ourselves
class MyEq a where

  eq :: a -> a -> Bool

  default eq :: (Generic a, GEq (Rep a)) => a -> a -> Bool -- XDefaultSignatures
  eq a b = geq (from a) (from b)

-- using generic metadata
data Person
  = Person
      { name :: String,
        age :: Int,
        phone :: Maybe String,
        permissions :: [Bool]
      }
  deriving (Generic)

class GJSONSchema (a :: Type -> Type) where
  gschema :: Writer [Text] Value

mergeObjects :: Value -> Value -> Value
mergeObjects (Object a) (Object b) = Object (a <> b)

emitRequired :: forall nm. KnownSymbol nm => Writer [Text] ()
emitRequired = tell . pure . pack . symbolVal $ Proxy @nm

type family RepName (x :: Type -> Type) :: Symbol where
  RepName (D1 ('MetaData nm _ _ _) _) = nm

type family TypeName (t :: Type) :: Symbol where
  TypeName t = RepName (Rep t)

type family ToJSONType (a :: Type) :: Symbol where
  ToJSONType Int = "integer"
  ToJSONType Integer = "integer"
  ToJSONType Float = "number"
  ToJSONType Double = "number"
  ToJSONType String = "string"
  ToJSONType Bool = "boolean"
  ToJSONType [a] = "array"
  ToJSONType a = TypeName a

makeTypeObj :: forall a. KnownSymbol (ToJSONType a) => Value
makeTypeObj = object ["type" .= String (pack . symbolVal $ Proxy @(ToJSONType a))]

makePropertyObj :: forall name. KnownSymbol name => Value -> Value
makePropertyObj v = object [pack (symbolVal $ Proxy @name) .= v]

instance
  (KnownSymbol nm, KnownSymbol (ToJSONType a)) =>
  GJSONSchema (M1 S ('MetaSel ('Just nm) _1 _2 _3) (K1 _4 a))
  where
  gschema = do
    emitRequired @nm
    pure . makePropertyObj @nm $ makeTypeObj @a
  {-# INLINE gschema #-}

instance (GJSONSchema f, GJSONSchema g) => GJSONSchema (f :*: g) where
  gschema =
    mergeObjects
      <$> gschema @f
      <*> gschema @g
  {-# INLINE gschema #-}

instance
  (TypeError ('Err.Text "JSON Schema does not support sum types")) =>
  GJSONSchema (f :+: g)
  where
  gschema = error "JSON Schema does not support sum types"
  {-# INLINE gschema #-}

instance GJSONSchema a => GJSONSchema (M1 C _1 a) where
  gschema = gschema @a
  {-# INLINE gschema #-}

instance
  (GJSONSchema a, KnownSymbol nm) =>
  GJSONSchema (M1 D ('MetaData nm _1 _2 _3) a)
  where
  gschema = do
    sch <- gschema @a
    pure $
      object
        [ "title" .= (String . pack . symbolVal $ Proxy @nm),
          "type" .= String "object",
          "properties" .= sch
        ]
  {-# INLINE gschema #-}

schema :: forall a. (GJSONSchema (Rep a), Generic a) => Value
schema =
  let (v, reqs) = runWriter $ gschema @(Rep a)
   in mergeObjects v $ object ["required" .= Array (fromList $ String <$> reqs)]
{-# INLINE schema #-}

instance
  {-# OVERLAPPING #-}
  ( KnownSymbol nm,
    KnownSymbol (ToJSONType a)
  ) =>
  GJSONSchema (M1 S ('MetaSel ('Just nm) _1 _2 _3) (K1 _4 (Maybe a)))
  where
  gschema = pure . makePropertyObj @nm $ makeTypeObj @a
  {-# INLINE gschema #-}

instance
  {-# OVERLAPPING #-}
  ( KnownSymbol nm,
    KnownSymbol (ToJSONType [a]),
    KnownSymbol (ToJSONType a)
  ) =>
  GJSONSchema (M1 S ('MetaSel ('Just nm) _1 _2 _3) (K1 _4 [a]))
  where
  gschema = do
    emitRequired @nm
    let innerType = object ["items" .= makeTypeObj @a]
    pure . makePropertyObj @nm . mergeObjects innerType $ makeTypeObj @[a]
  {-# INLINE gschema #-}

instance
  {-# OVERLAPPING #-}
  (KnownSymbol nm) =>
  GJSONSchema (M1 S ('MetaSel ('Just nm) _1 _2 _3) (K1 _4 String))
  where
  gschema = do
    emitRequired @nm
    pure . makePropertyObj @nm $ makeTypeObj @String
  {-# INLINE gschema #-}

pp :: ToJSON a => a -> IO ()
pp = LC8.putStrLn . encodePretty

-- * Chapter13> pp (schema @Person) 🎉🎉🎉
-- {
--     "required": [
--         "name",
--         "age",
--         "permissions"
--     ],
--     "title": "Person",
--     "type": "object",
--     "properties": {
--         "phone": {
--             "type": "string"
--         },
--         "age": {
--             "type": "integer"
--         },
--         "name": {
--             "type": "string"
--         },
--         "permissions": {
--             "items": {
--                 "type": "boolean"
--             },
--             "type": "array"
--         }
--     }
-- }

-- performance

mySchema :: Value
mySchema = schema @Person

inspect $ hasNoGenerics 'mySchema

-- Kan Extensions 🤯🤯🤯
newtype Yoneda f a
  = Yoneda
      { runYoneda :: forall b. (a -> b) -> f b
      }

-- Data.Functor.Yoneda == forall f. Functor f => f a
-- Data.Functor.Day.Curried == forall f. Applicative f => f a
-- Control.Monad.Codensity == forall f. Monad f => f a
instance Functor (Yoneda f) where
  fmap f (Yoneda y) = Yoneda $ \k -> y (k . f)
