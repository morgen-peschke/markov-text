package peschke.markov.utils

import cats.{Id, Show}
import cats.syntax.functor._
import cats.syntax.applicative._

/**
  * Having this helps mock out the random bits for repeatable tests.
  * A nice side-effect is that, by printing the seed, the user can regenerate output
  * they particularly liked.
  */
trait RandomNumberGenerator[A] {
  def next(seed: A): (A, Long)

  /**
    * Bounds the random number by `max`, so the range is `[0, max]`
    * Thank you: https://stackoverflow.com/a/10984975
    *
    * @param seed previous seed
    * @param max  inclusive upper bound
    * @return next seed and value
    */
  def next(seed: A, max: PositiveInt): (A, PositiveInt) = {
    val limit = Long.MaxValue - (Long.MaxValue % max.value)
    val (resultSeed, resultRaw) = Iterator
      .iterate(next(seed)) {
        case (s, _) => next(s)
      }
      .dropWhile(_._2 >= limit)
      .next()

    (resultSeed, PositiveInt.valueOf((resultRaw % max.value).toInt + 1).getOrElse(PositiveInt.One))
  }

  /**
    * Bounds the random number by `min` and `max`, so the range is `[min, max]`
    *
    * If `min` is less than `max`, the range will instead be `[max, min]`
    *
    * @param seed previous seed
    * @param min  inclusive lower bound
    * @param max  inclusive upper bound
    * @return next seed and value
    */
  def next(seed: A, min: PositiveInt, max: PositiveInt): (A, PositiveInt) =
    (max distance min).fold((seed, min)) { range =>
      val (seedN, value) = next(seed, range)
      (seedN, value + min)
    }
}

object RandomNumberGenerator {
  def apply[A](implicit R: RandomNumberGenerator[A]): RandomNumberGenerator[A] = R

  def instance[A](f: A => (A, Long)): RandomNumberGenerator[A] = (seed: A) => f(seed)

  final case class Seed(value: Long)
  // Thank you: https://en.wikipedia.org/wiki/Xorshift#xorshift*
  implicit val XORShift: RandomNumberGenerator[Seed] = RandomNumberGenerator.instance[Seed] { seed =>
    def xorUsing(f: Long => Long): Long => Id[Long] = l => l ^ f(l)

    val newSeed =
      seed.value.pure[Id]
        .map(xorUsing(_ >> 12))
        .map(xorUsing(_ << 25))
        .map(xorUsing(_ >> 27))
        .map(Seed(_))

    (newSeed, newSeed.value * 0x2545F4914F6CDD1DL)
  }
  object Seed {
    implicit val show: Show[Seed] = Show.show(_.value.toString)
  }
}