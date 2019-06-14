package peschke.markov.utils

import cats.Show
import cats.data.NonEmptyList
import fs2.Stream
import peschke.markov.utils.syntax._

import scala.language.higherKinds
import scala.util.matching.Regex

/**
  * Mostly for formatting the output, but it's also used a bit while processing the input.
  */
final case class Sentence(words: NonEmptyList[String]) {
  def stream[F[_]]: Stream[F, String] = Stream.emits(words.toList)

  def length: PositiveInt = PositiveInt.lengthOf(words)
}
object Sentence {
  val WordSep: Regex = "\\s".r

  def from(line: String): Option[Sentence] =
    NonEmptyList.fromList(WordSep.split(line).map(_.trim).filterNot(_.isEmpty).toList).map(Sentence(_))

  /**
    * Title-cases the first word, and adds a trailing `.` if there isn't already one.
    */
  implicit val show: Show[Sentence] = Show.show {
    case Sentence(NonEmptyList(first, rest)) =>
      val buffer = new StringBuilder
      buffer.append(first.toTitleCase)
      rest.foreach(buffer.append(" ").append(_))
      val rendered = buffer.toString()
      if (rendered.endsWith(".")) rendered else s"$rendered."
  }
}