import arrow.test.UnitSpec
import learn.org.thinking_with_types.one.CHIsomorphism
import learn.org.thinking_with_types.one.curry
import learn.org.thinking_with_types.one.uncurry

class ChapterOneTest : UnitSpec() {
  init {
    testLaws(CHIsomorphism.proof({ it.curry() }, { it.uncurry() }))
  }
}