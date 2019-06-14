package peschke.markov

import cats.syntax.option._
import cats.syntax.applicative._
import org.scalatest.concurrent.{Signaler, ThreadSignaler, TimeLimits}
import org.scalatest.{MustMatchers, OptionValues, WordSpec}
import cats.instances.char._
import cats.instances.int._
import cats.Monoid
import org.scalatest.time.{Seconds, Span}
import peschke.markov.factories.dsl._
import peschke.markov.utils.PositiveInt.{One, Three, Two, Five, Ten}
import peschke.markov.utils.{FilledFIFO, PositiveInt}

class SentenceGeneratorTest extends WordSpec with MustMatchers with TimeLimits with OptionValues {
  val Four: PositiveInt = Three.next

  "SentenceGenerator.nextWord" when {
    "there are alternatives for the current key" should {
      "select the correct set of alternatives" in {
        val generator = SentenceGenerator(
          forrest(
            One -> chain(
              State.of("a") -> alternatives("a" <# One),
              State.of("b") -> alternatives("b" <# One)),
            Two -> chain(
              State.of("a", "b") -> alternatives("c" <# One),
              State.of("b", "c") -> alternatives("d" <# One))))

        val seed0 = 'a'
        val seed1 = 'b'
        val rng = DummyRNG.fixed(seed1, 1L)

        generator.nextWord(FilledFIFO("a"), seed0)(rng) mustBe(seed1, "a".some)
        generator.nextWord(FilledFIFO("b"), seed0)(rng) mustBe(seed1, "b".some)
        generator.nextWord(FilledFIFO("a", "b"), seed0)(rng) mustBe(seed1, "c".some)
        generator.nextWord(FilledFIFO("b", "c"), seed0)(rng) mustBe(seed1, "d".some)
      }

      "return one of the values, using the RNG" in {
        val generator = SentenceGenerator(
          forrest(
            One -> chain(
              State.of("key") -> alternatives(
                "a" <# One,
                "b" <# One,
                "c" <# Two,
                "d" <# Three))))

        val chain0 = "key".pure[FilledFIFO]

        generator.nextWord(chain0, 'a')(DummyRNG.fixed('b', 1L)) mustBe('b', "a".some)
        generator.nextWord(chain0, 'a')(DummyRNG.fixed('b', 2L)) mustBe('b', "b".some)
        generator.nextWord(chain0, 'a')(DummyRNG.fixed('b', 3L)) mustBe('b', "c".some)
        generator.nextWord(chain0, 'a')(DummyRNG.fixed('b', 4L)) mustBe('b', "c".some)
        generator.nextWord(chain0, 'a')(DummyRNG.fixed('b', 5L)) mustBe('b', "d".some)
        generator.nextWord(chain0, 'a')(DummyRNG.fixed('b', 6L)) mustBe('b', "d".some)
        generator.nextWord(chain0, 'a')(DummyRNG.fixed('b', 7L)) mustBe('b', "d".some)
      }

      "behave reasonably when there is only one alternative" in {
        val generator = SentenceGenerator(forrest(One -> chain(State.of("a") -> alternatives("a" <# One))))

        val seed0 = 'a'
        val seed1 = 'b'
        val rng = DummyRNG.fixed(seed1, 1L)

        generator.nextWord(FilledFIFO("a"), seed0)(rng) mustBe(seed1, "a".some)
      }
    }

    "there are no alternatives for the current key" should {
      "shrink until a key with alternatives is found" in {
        val generator = SentenceGenerator(
          forrest(
            One -> chain(State.of("a") -> alternatives("a" <# One)),
            Two -> chain(State.of("b", "c") -> alternatives("b" <# One)),
            Three -> chain(State.of("c", "d", "e") -> alternatives("c" <# One))
          ))

        val seed0 = 'a'
        val seed1 = 'b'
        val rng = DummyRNG.fixed(seed1, 1L)

        generator.nextWord(FilledFIFO("a", "b", "c"), seed0)(rng) mustBe(seed1, "b".some)
        generator.nextWord(FilledFIFO("y", "z", "a"), seed0)(rng) mustBe(seed1, "a".some)
      }

      "return None if shrinking fails to find a valid key" in {
        val generator = SentenceGenerator(
          forrest(
            One -> chain(State.of("a") -> alternatives("a" <# One)),
            Two -> chain(State.of("b", "c") -> alternatives("b" <# One)),
            Three -> chain(State.of("c", "d", "e") -> alternatives("c" <# One))
          ))

        val seed0 = 'a'
        val rng = DummyRNG.fixed('b', 1L)

        generator.nextWord(FilledFIFO("x", "y", "z"), seed0)(rng) mustBe(seed0, none)
      }

      "behave reasonably when there is only one alternative" in {
        val generator = SentenceGenerator(forrest(One -> chain(State.of("a") -> alternatives("a" <# One))))

        val seed0 = 'a'
        val rng = DummyRNG.fixed('b', 1L)

        generator.nextWord(FilledFIFO("b"), seed0)(rng) mustBe(seed0, none)
      }

      "behave reasonably when there are no alternatives" in {
        val generator = SentenceGenerator(Monoid[MarkovForrest].empty)

        val seed0 = 'a'
        val rng = DummyRNG.fixed('b', 1L)

        generator.nextWord(FilledFIFO("a"), seed0)(rng) mustBe(seed0, none)
      }
    }
  }

  "SentenceGenerator.generate" should {
    "not hang if a sentence of the desired length cannot be generated" in {
      implicit val signaler: Signaler = ThreadSignaler

      val generator = SentenceGenerator(
        forrest(
          Two -> chain(
            State.of("a", "b") -> alternatives("c" <# One),
            State.of("b", "c") -> alternatives("a" <# One)
          )
        ))

      val seedIn = 0
      val seedOut = 3
      val rng = DummyRNG.transitions(
        seedIn -> 1L -> 1,
        1 -> 1L -> 2,
        2 -> 1L -> seedOut
      )

      failAfter(Span(10, Seconds)) {
        generator.generate(Ten, seedIn)(rng) mustBe(seedOut, sentence("a", "b", "c", "a").some)
      }
    }

    "generate a sentence of the desired length, if shrinking is never needed" in {
      val generator = SentenceGenerator(
        forrest(
          Two -> chain(
            State.of("a", "b") -> alternatives("c" <# One),
            State.of("b", "c") -> alternatives("a" <# One),
            State.of("c", "a") -> alternatives("b" <# One)
          )
        ))

      val seedIn = 0
      val seedOut = 4
      val rng = DummyRNG.transitions(
        seedIn -> 1L -> 1,
        1 -> 1L -> 2,
        2 -> 1L -> 3,
        3 -> 1L -> seedOut
      )

      generator.generate(Five, seedIn)(rng) mustBe(seedOut, sentence("a", "b", "c", "a", "b").some)
    }

    "generate a sentence of desired length only shrinking when needed" in {
      val generator = SentenceGenerator(
        forrest(
          One -> chain(State.of("a") -> alternatives("b" <# One)),
          Two -> chain(
            State.of("a", "b") -> alternatives("c" <# One),
            State.of("b", "c") -> alternatives("a" <# One)
          )
        ))

      val seedIn = 0
      val seedOut = 4
      val rng = DummyRNG.transitions(
        seedIn -> 1L -> 1,
        1 -> 1L -> 2,
        2 -> 1L -> 3,
        3 -> 1L -> seedOut
      )

      generator.generate(Five, seedIn)(rng) mustBe(seedOut, sentence("a", "b", "c", "a", "b").some)
    }
  }
}
