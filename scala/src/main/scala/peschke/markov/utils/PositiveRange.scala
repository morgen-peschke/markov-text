package peschke.markov.utils

import cats.{Order, Show}
import cats.data.{NonEmptyVector, ValidatedNel}
import cats.syntax.apply._
import cats.syntax.show._
import cats.syntax.validated._
import com.github.ghik.silencer.silent
import com.monovore.decline.Argument

/**
  * Manly used to encode ranges for the CLI parameters
  *
  * @param min the inclusive lower bound
  * @param max the inclusive upper bound
  */
case class PositiveRange private[utils](min: PositiveInt, max: PositiveInt) {
  def toNEV: NonEmptyVector[PositiveInt] =
    NonEmptyVector(min, Iterator.iterate(min.next)(_.next).takeWhile(_.value <= max.value).toVector)

  @silent private[this] def copy(): PositiveRange = ???
}
object PositiveRange {
  @silent private[this] def apply(min: PositiveInt, max: PositiveInt): PositiveRange = ???

  /**
    * Creates a [[peschke.markov.utils.PositiveRange]], ensuring that `a` < `b`
    */
  def between(a: PositiveInt, b: PositiveInt): PositiveRange =
    new PositiveRange(
      Order[PositiveInt].min(a, b),
      Order[PositiveInt].max(a, b))

  implicit val show: Show[PositiveRange] = Show.show(r => show"${r.min}..${r.max}")
  implicit val argument: Argument[PositiveRange] = new Argument[PositiveRange] {
    private val PI = Argument[PositiveInt]
    type ValidatedArg[A] = ValidatedNel[String, A]

    override def read(string: String): ValidatedNel[String, PositiveRange] =
      string.split('.') match {
        case Array(rawLow, "", rawHigh) =>
          (PI.read(rawLow).leftMap(_.map("low " + _)): ValidatedArg[PositiveInt],
            PI.read(rawHigh).leftMap(_.map("high " + _)): ValidatedArg[PositiveInt]
            ).mapN(_ to _)

        case _ => "must be '<low>..<high>', where <low> and <high> are positive integers".invalidNel
      }

    override def defaultMetavar: String = "min..max"
  }
}
