package peschke.markov.utils

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{MustMatchers, WordSpec}
import peschke.markov.utils.RandomNumberGenerator.Seed

import scala.annotation.tailrec
import scala.math.BigDecimal.RoundingMode

class RandomNumberGeneratorXORShiftTest extends WordSpec with MustMatchers with GeneratorDrivenPropertyChecks {

  import org.scalacheck.Shrink

  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

  val Pi = BigDecimal(Math.PI)
  val MaxValue = BigDecimal(Long.MaxValue)
  val Zero = BigDecimal(0)
  val One = BigDecimal(1)
  val Two = BigDecimal(2)
  val Four = BigDecimal(4)
  val SqrtErrorThreshold: BigDecimal = One / BigDecimal(10000000)

  def pair(seed: Seed): (Seed, (BigDecimal, BigDecimal)) = {
    val (s0, x) = RandomNumberGenerator[Seed].next(seed)
    val (s1, y) = RandomNumberGenerator[Seed].next(s0)
    s1 -> (BigDecimal(x) / MaxValue, BigDecimal(y) / MaxValue)
  }

  /**
    * Implementation of https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Babylonian_method
    */
  def sqrt(bd: BigDecimal, scale: Int): BigDecimal = {
    val input = bd.setScale(scale + 3, RoundingMode.HALF_EVEN)

    @tailrec
    def loop(previous: BigDecimal): BigDecimal = {
      val next = ((input / previous) + previous) / Two
      val error = ((input - next.pow(2)).abs / (Two * next)).setScale(SqrtErrorThreshold.scale, RoundingMode.HALF_EVEN)
      if (error < SqrtErrorThreshold) next
      else loop(next.setScale(scale + 3, RoundingMode.HALF_EVEN))
    }

    loop(input).setScale(scale, RoundingMode.HALF_EVEN)
  }

  "RandomNumberGeneratorXORShiftTest.sqrt" should {
    "pass sanity checks" in {
      sqrt(BigDecimal(4), 2) mustBe BigDecimal(2)
    }

    "pass property checks" in forAll(Gen.choose(1, Long.MaxValue).map(BigDecimal(_))) { input =>
      sqrt(input.pow(2), input.precision).setScale(0, RoundingMode.DOWN) must be(input +- SqrtErrorThreshold)
    }
  }

  // Based on https://asecuritysite.com/encryption/mc
  trait MonteCarloPiTest {
    val NumberOfTrials = BigDecimal(9000)

    def streamOfPoints: Stream[(BigDecimal, BigDecimal)]

    def calculatePI: BigDecimal =
      streamOfPoints
        .take(NumberOfTrials.toIntExact)
        .count {
          case (x, y) => sqrt(x.pow(2) + y.pow(2), 32).setScale(32) <= One
        } * Four / NumberOfTrials
  }
  object MonteCarloPiTest {
    def reference: MonteCarloPiTest = new MonteCarloPiTest {
      override def streamOfPoints: Stream[(BigDecimal, BigDecimal)] = {
        val rng = new scala.util.Random()
        Stream.continually {
          (BigDecimal(rng.nextLong()) / MaxValue, BigDecimal(rng.nextLong()) / MaxValue)
        }
      }

    }
  }


  "RandomNumberGenerator.XORShift" should {
    "pass the Monte Carlo PI test" in new MonteCarloPiTest {
      // This is just for messing around, so we're only asking it to be half as good as scala.util.Random
      val errorThreshold: BigDecimal = (Pi - MonteCarloPiTest.reference.calculatePI).abs * Two
      override def streamOfPoints: Stream[(BigDecimal, BigDecimal)] =
        Stream
          .iterate(pair(Seed.random))(prev => pair(prev._1))
          .map(_._2)

      calculatePI mustBe (Pi +- errorThreshold)
    }
  }
}
