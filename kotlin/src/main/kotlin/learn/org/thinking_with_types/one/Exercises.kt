package learn.org.thinking_with_types.one

import arrow.core.Either
import arrow.core.Tuple2
import arrow.core.andThen
import arrow.core.compose
import arrow.core.extensions.eq
import arrow.core.left
import arrow.core.right
import arrow.core.toT
import arrow.test.generators.functionAAToA
import arrow.test.laws.Law
import arrow.test.laws.equalUnderTheLaw
import io.kotlintest.properties.Gen
import io.kotlintest.properties.forAll

/**
 * Determine Cardinality of: Ex: 1.2.i
 * Either Bool (Bool, Maybe Bool) -> Bool // Cardinality Rule of Either  |Either a b| = |a| + |b|
 *     |Bool| + |(Bool, Maybe Bool)| -> Bool  // Cardinality Rule of Function Types |a -> b| = |b| ^ |a|
 *     |Bool| ^ (|Bool| + |(Bool, Maybe Bool)|) // Algebra
 *     |Bool| ^ |Bool| * |Bool| ^ |(Bool, Maybe Bool)|  // Cardinality of Maybe |Maybe a| = 1 + |a| and Product Types |(a, b)| = |a| × |b|
 *     |Bool| ^ |Bool| * |Bool| ^ (|Bool| * (1 + |Bool|)) //  = 256 🚀
 */

/**
 * This might not be the most intuitive rule:
 *  |a -> b| = |b| ^ |a|   it means for every a there needs to be a b
 */

/**
 * Exercise 1.4-i
 */
fun <P1, P2, R> ((P1, P2) -> R).curry(): (P1) -> (P2) -> R =
  { p1 -> { p2 -> invoke(p1, p2) } }

fun <P1, P2, R> ((P1) -> (P2) -> R).uncurry(): (P1, P2) -> R =
  { p1, p2 -> invoke(p1).invoke(p2) }

/**
 * We then define property tests with generated Integers
 */
object CHIsomorphism {
  fun proof(
    from: ((Int, Int) -> Int) -> (Int) -> (Int) -> Int,
    to: ((Int) -> (Int) -> Int) -> (Int, Int) -> Int
  ): List<Law> =
    listOf(Law("Curry–Howard proof") {
      forAll(Gen.functionAAToA(Gen.int()), Gen.int(), Gen.int()) { f: (Int, Int) -> Int, p1: Int, p2: Int ->
        from(f).invoke(p1).invoke(p2).equalUnderTheLaw(to(from(f)).invoke(p1, p2), Int.eq())
      }
    })
}

/**
 * Exercise 1.4-ii
 * Proof: a^b × a^c = a^b+c
 */
fun <A, B, C> productRuleTo(f: (B) -> A, g: (C) -> A): (Either<B, C>) -> A =
  { either -> either.fold(f, g) }

fun <A, B, C> productRuleFrom(f: (Either<B, C>) -> A): Tuple2<(B) -> A, (C) -> A> =
  f compose { b: B -> b.left() } toT f.compose { c: C -> c.right() }

/**
 * 1.4-iii
 * Proof: (a×b)^c = a^c × b^c
 */
fun <A, B, C> to(f: (C) -> Tuple2<A, B>): Tuple2<(C) -> A, (C) -> B> =
  f andThen { it.a } toT f.andThen { it.b }

fun <A, B, C> from(f: (C) -> A, g: (C) -> B): (C) -> Tuple2<A, B> =
  { f(it) toT g(it) }