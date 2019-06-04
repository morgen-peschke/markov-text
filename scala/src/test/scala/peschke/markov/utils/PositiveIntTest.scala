package peschke.markov.utils

import cats.data.NonEmptyVector
import org.scalatest.{MustMatchers, OptionValues, WordSpec}

class PositiveIntTest extends WordSpec with MustMatchers with OptionValues {
  "PositiveInt.valueOf" should {
    "only construct for positive integers" in {
      PositiveInt.valueOf(-1) mustBe None
      PositiveInt.valueOf(0) mustBe None
      PositiveInt.valueOf(1).map(_.value) mustBe Some(1)
    }
  }

  "PositiveInt.previous" should {
    "stop returning values when they reach zero" in {
      val v2 = PositiveInt.valueOf(3).value.previous
      v2.map(_.value) mustBe Some(2)

      val v1 = v2.flatMap(_.previous)
      v1.map(_.value) mustBe Some(1)

      val v0 = v1.flatMap(_.previous)
      v0 mustBe None
    }
  }

  "PositiveInt.distance" should {
    "return None if the values are equal" in {
      (PositiveInt.Five distance PositiveInt.Five) mustBe None
    }

    "return the distance between the two values" in {
      (PositiveInt.Five distance PositiveInt.Three) mustBe Some(PositiveInt.Two)
      (PositiveInt.Five distance PositiveInt.Two) mustBe Some(PositiveInt.Three)
    }

    "not choke if the rhs is larger than the lhs" in {
      (PositiveInt.Three distance PositiveInt.Five) mustBe Some(PositiveInt.Two)
      (PositiveInt.Two distance PositiveInt.Five) mustBe Some(PositiveInt.Three)
    }
  }

  "PositiveInt.to" should {
    "return all values from lhs to rhs (inclusive)" in {
      (PositiveInt.One to PositiveInt.valueOf(3).value).toNEV.map(_.value) mustBe NonEmptyVector.of(1, 2, 3)
    }
  }

  "PositiveInt" should {
    "have the expected behavior for equality and hashCode" in {
      PositiveInt.One mustBe PositiveInt.valueOf(1).value
      PositiveInt.valueOf(7000).value mustBe PositiveInt.valueOf(7000).value
      PositiveInt.valueOf(25).hashCode() mustBe PositiveInt.valueOf(25).hashCode()
      PositiveInt.valueOf(90).hashCode() must not be 90.hashCode()
    }
  }
}
