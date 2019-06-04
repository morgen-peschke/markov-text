package peschke.markov

import cats.Id
import cats.kernel.Monoid
import fs2.Stream
import org.scalatest.{MustMatchers, OptionValues, WordSpec}
import peschke.markov.utils.PositiveInt
import peschke.markov.factories.dsl._

class MarkovForrestTest extends WordSpec with MustMatchers with OptionValues {
  val MaxDepth: PositiveInt = PositiveInt.Three

  "Possibilities.from" should {
    "Not add words until at least one key is full" in {
      val actual = MarkovForrest.from[Id](MaxDepth, Stream.emit("This"))
      val expected = Monoid[MarkovForrest].empty

      actual mustBe expected
    }

    "Only add words when a full key is available" in {
      val actual = MarkovForrest.from[Id](MaxDepth, Stream("This", "is", "a", "test"))
      val expected = forrest(
        PositiveInt.One -> chain(
          State.of("This") -> alternatives("is" <# PositiveInt.One),
          State.of("is") -> alternatives("a" <# PositiveInt.One),
          State.of("a") -> alternatives("test" <# PositiveInt.One)
        ),
        PositiveInt.One.next -> chain(
          State.of("This", "is") -> alternatives("a" <# PositiveInt.One),
          State.of("is", "a") -> alternatives("test" <# PositiveInt.One)
        ),
        PositiveInt.One.next.next -> chain(
          State.of("This", "is", "a") -> alternatives("test" <# PositiveInt.One))
      )

      actual mustBe expected
    }

    "Use the number of times a word appears after a key as it's weight" in {
      val actual = MarkovForrest.from[Id](MaxDepth, Stream(
        "a_", "b_", "c_",
        "a_", "b_", "d_",
        "a_", "c_", "b_"
      ))

      val expected = forrest(
        PositiveInt.One -> chain(
          State.of("a_") -> alternatives("c_" <# PositiveInt.One, "b_" <# PositiveInt.Two),
          State.of("b_") -> alternatives("c_" <# PositiveInt.One, "d_" <# PositiveInt.One),
          State.of("c_") -> alternatives("a_" <# PositiveInt.One, "b_" <# PositiveInt.One),
          State.of("d_") -> alternatives("a_" <# PositiveInt.One)),
        PositiveInt.Two -> chain(
          State.of("a_", "b_") -> alternatives("c_" <# PositiveInt.One, "d_" <# PositiveInt.One),
          State.of("a_", "c_") -> alternatives("b_" <# PositiveInt.One),
          State.of("b_", "c_") -> alternatives("a_" <# PositiveInt.One),
          State.of("b_", "d_") -> alternatives("a_" <# PositiveInt.One),
          State.of("c_", "a_") -> alternatives("b_" <# PositiveInt.One),
          State.of("d_", "a_") -> alternatives("c_" <# PositiveInt.One))
        ,
        PositiveInt.Three -> chain(
          State.of("a_", "b_", "c_") -> alternatives("a_" <# PositiveInt.One),
          State.of("a_", "b_", "d_") -> alternatives("a_" <# PositiveInt.One),
          State.of("b_", "c_", "a_") -> alternatives("b_" <# PositiveInt.One),
          State.of("b_", "d_", "a_") -> alternatives("c_" <# PositiveInt.One),
          State.of("c_", "a_", "b_") -> alternatives("d_" <# PositiveInt.One),
          State.of("d_", "a_", "c_") -> alternatives("b_" <# PositiveInt.One)))

      actual mustBe expected
    }
  }
}
