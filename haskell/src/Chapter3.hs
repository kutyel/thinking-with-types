module Chapter3 where

-- exercise 3-i
newtype T1 a = T1 (Int -> a) -- covariant (+ = +)

instance Functor T1 where
  fmap f (T1 g) = T1 (f . g)

newtype T2 a = T2 (a -> Int) -- contravariant (- = -)

newtype T3 a = T3 (a -> a) -- invariant (-+ = -+)

newtype T4 a = T4 ((Int -> a) -> Int) -- contravariant (+◦- = -)

newtype T5 a = T5 ((a -> Int) -> Int) -- covariant (-◦- = +)

instance Functor T5 where
  fmap f (T5 aii) = T5 (\g -> aii $ g . f)

-- Functor instances can only be covariant!
