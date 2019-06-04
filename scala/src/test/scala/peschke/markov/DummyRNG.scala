package peschke.markov

import cats.Show
import cats.syntax.show._
import cats.syntax.option._
import cats.instances.long._
import peschke.markov.utils.{PositiveInt, RandomNumberGenerator}

case class DummyRNG[S: Show](transitions: S => Option[(S, Long)]) extends RandomNumberGenerator[S] {
  override def next(seed: S): (S, Long) =
    transitions(seed).getOrElse {
      throw new IllegalStateException(s"Ran out of entropy and don't know what to do with $seed")
    }

  override def next(seed: S, max: PositiveInt): (S, PositiveInt) = {
    val (nextSeed, nextLong) = next(seed)
    if (nextLong > max.value.toLong)
      throw new IllegalStateException(
        show"Faulty setup: should not have returned a value larger than $max for $seed, but returned $nextLong")
    else nextSeed -> PositiveInt.valueOf(nextLong.toInt).getOrElse(
      throw new IllegalStateException(
        show"Faulty setup: should not have returned a positive value for $seed, but returned $nextLong"
      )
    )
  }
}
object DummyRNG {
  def fixed[S: Show](seed: S, value: Long): RandomNumberGenerator[S] = {
    val ret = (seed, value).some
    new DummyRNG[S](_ => ret)
  }

  def transitions[S: Show](t0: ((S, Long), S), ts: ((S, Long), S)*): RandomNumberGenerator[S] =
    new DummyRNG(
      (t0 :: ts.toList)
        .map {
          case ((in, next), out) => (in, (out, next))
        }
        .toMap.get(_)
    )
}
