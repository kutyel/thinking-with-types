package learn.org.thinking_with_types.one

import arrow.Kind
import arrow.extension

/**
 * Here is another example of an isomorphism where the morphisms are natural transformations
 * This behavior is defined as a typeclass.
 * [alpha] and [beta] do the same thing as the `to` and `from` function from the 1 chapter
 * If you want to know more about this typeclass read Chapter 14.2 Representable Functors
 * in category theory for programmers you can get it for free here:
 * https://github.com/hmemcpy/milewski-ctfp-pdf
 */
interface Representable<F, Key> {
  fun <A> alpha(f: (Key) -> A): Kind<F, A>
  fun <A> Kind<F, A>.beta(key: Key): A
}


/**
 * Exercise 1.extra: Implement this typeclass instance
 */
@extension
interface PairRepresentable<K, V> : Representable<Pair<K, V>, K>


