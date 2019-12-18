module Chapter1 where

-- Exercise 1.2-i
-- Determine the cardinality of `Either Bool (Bool, Maybe Bool) -> Bool`.

-- | Either Bool (Bool, Maybe Bool) -> Bool|
--  |Bool|^|Either Bool (Bool, Maybe Bool)|
--  |Bool|^(|Bool| + |(Bool, Maybe Bool)|)
--  |Bool|^(|Bool| + (|Bool| × |Maybe Bool|))
--  |Bool|^(|Bool| + (|Bool| × (1 + |Bool|)))
--  2^(2 + (2 × (1 + 2)))
--  2^(2 + (2 × 3))
--  2^(2 + 6)
--  2^8
--  > 256! 🚀
data Three = One | Two | Three
  deriving (Eq, Ord, Enum, Bounded)

-- 🤯🤯🤯
newtype TicTacToe a
  = TicTacToe
      { board :: Three -> Three -> a
      }

emptyBoard :: TicTacToe (Maybe Bool)
emptyBoard = TicTacToe $ const $ const Nothing

-- Exercise 1.4-i
-- Use Curry–Howard to prove that (a^b)^c = a^b×c.
-- That is, provide a function of type `(b -> c -> a) -> (b, c) -> a`,
-- and one of `((b, c) -> a) -> b -> c -> a`. Makesure they satisfy
-- the equalities `to . from = id` and `from . to = id`.
-- Do these functions remind you of anything from Prelude?
uncurry :: (c -> b -> a) -> (b, c) -> a
uncurry f (x, y) = f y x

curry :: ((b, c) -> a) -> c -> b -> a
curry f x y = f (y, x)

-- TODO: property-test that: from . to == id
-- TODO: property-test that: to . from == id

-- Exercise 1.4-ii
-- Give a proof of the exponent law that: a^b × a^c = a^b+c.
productRuleTo :: (b -> a) -> (c -> a) -> Either b c -> a
productRuleTo f _ (Left x) = f x
productRuleTo _ g (Right x) = g x

-- productRuleTo == either 🤯

productRuleFrom :: (Either b c -> a) -> (b -> a, c -> a)
productRuleFrom f = (f . Left, f . Right)

-- Exercise 1.4-iii
-- Prove (a×b)^c = a^c × b^c
proofTo :: (c -> (a, b)) -> (c -> a, c -> b)
proofTo f = (fst . f, snd . f)

proofFrom :: (c -> a) -> (c -> b) -> c -> (a, b)
proofFrom f g x = (f x, g x)
