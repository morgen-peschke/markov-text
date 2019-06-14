package peschke.markov.utils

import cats.data.NonEmptyVector
import org.scalatest.{MustMatchers, WordSpec}
import peschke.markov.utils.PositiveInt.{Five, One, Three, Two}

class PositiveRangeTest extends WordSpec with MustMatchers {
  "PositiveRange.between" should {
    "reorder the arguments only if needed" in {
      PositiveRange.between(One, Five) must matchPattern { case PositiveRange(One, Five) => }
      PositiveRange.between(Five, One) must matchPattern { case PositiveRange(One, Five) => }
    }
  }

  "PositiveRange.toNEV" should {
    "emit a single value if min == max" in {
      PositiveRange.between(One, One).toNEV mustBe NonEmptyVector.of(One)
    }

    "emit the expected values" in {
      PositiveRange.between(One, Three).toNEV mustBe NonEmptyVector.of(One, Two, Three)
    }
  }
}
