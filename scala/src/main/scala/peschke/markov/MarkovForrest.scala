package peschke.markov

import cats.data.{NonEmptyList, NonEmptyMap, NonEmptyVector}
import cats.instances.string._
import cats.instances.tuple._
import cats.instances.either._
import cats.instances.vector._
import cats.syntax.foldable._
import cats.syntax.option._
import cats.syntax.either._
import cats.syntax.show._
import cats.{Functor, Monoid, Order, Show}
import fs2.Stream
import io.circe.{Decoder, Encoder, Json}
import peschke.markov.utils.syntax._
import peschke.markov.utils.{FIFO, KeyAndValue, PositiveInt}
import io.circe.generic.semiauto._

import scala.collection.immutable.SortedMap
import scala.language.higherKinds

final case class Choice(word: String, weight: PositiveInt) {
  def increment: Choice = copy(weight = weight.next)
}
object Choice {
  implicit val show: Show[Choice] = Show.show(p => s"""${p.weight.show}x "${p.word}"""")
  implicit val order: Order[Choice] = Order.by(p => p.weight -> p.word)
  implicit val encoder: Encoder[Choice] = deriveEncoder
  implicit val decoder: Decoder[Choice] = deriveDecoder
}

final case class Alternatives(alternatives: NonEmptyVector[Choice]) {
  val totalWeight: PositiveInt = alternatives.map(_.weight).reduce
}
object Alternatives {
  implicit val show: Show[Alternatives] = Show.show(c => c.alternatives.toVector.map(_.show).mkString("{", "|", "}"))
  implicit val encoder: Encoder[Alternatives] = deriveEncoder
  implicit val decoder: Decoder[Alternatives] = deriveDecoder
}

final case class State(parts: NonEmptyVector[String])
object State {
  def of(p0: String, ps: String*): State = State(NonEmptyVector(p0, ps.toVector))

  implicit val order: Order[State] = Order.by(_.parts.toVector)
  implicit val ordering: Ordering[State] = order.toOrdering
  implicit val show: Show[State] = Show.show(_.parts.toVector.mkString("[", ",", "]"))
  implicit val encoder: Encoder[State] = deriveEncoder
  implicit val decoder: Decoder[State] = deriveDecoder
}

final case class MarkovChain(transitions: NonEmptyMap[State, Alternatives]) extends AnyVal {
  def keyLength: Option[PositiveInt] =
    transitions.keys.toNonEmptyList.map(_.parts).map(PositiveInt.lengthOf[NonEmptyVector, String]).distinct match {
      case NonEmptyList(allLengthsAreTheSame, Nil) => allLengthsAreTheSame.some
      case lengthsDoNotMatch                       =>
        throw new IllegalStateException(show"Lengths (${lengthsDoNotMatch.mkString_(", ")}) do not match in: $this")
    }
}
object MarkovChain {
  implicit val show: Show[MarkovChain] = Show.show { l =>
    val sorted = l.transitions.toNel.sortBy(_._1)
    show"MarkovChain(${sorted.mkString_(", ")})"
  }
  implicit val encoder: Encoder[MarkovChain] = {
    val KV = Encoder[KeyAndValue[State, Alternatives]]
    Encoder.instance[MarkovChain] { mc =>
      Json.arr(mc.transitions.toNel.toList.map {
        (KeyAndValue.fromTuple[State, Alternatives] _).andThen(KV(_))
      }: _*)
    }
  }
  implicit val decoder: Decoder[MarkovChain] = {
    val KV = Decoder[KeyAndValue[State, Alternatives]]
    Decoder.decodeNonEmptyList[Json].emap(
      _.traverse[Decoder.Result, KeyAndValue[State, Alternatives]](KV.decodeJson)
        .map(_.map(_.toTuple).toNem)
        .bimap(_.message, MarkovChain(_)))
  }
}

final case class MarkovForrest(chains: Map[PositiveInt, MarkovChain]) extends AnyVal
object MarkovForrest {
  implicit val monoid: Monoid[MarkovForrest] = new Monoid[MarkovForrest] {
    override val empty: MarkovForrest = MarkovForrest(Map.empty)

    override def combine(x: MarkovForrest,
                         y: MarkovForrest): MarkovForrest = MarkovForrest(x.chains ++ y.chains)
  }

  implicit val encoder: Encoder[MarkovForrest] = Encoder.instance { mf =>
    Encoder[Map[PositiveInt, MarkovChain]].apply(mf.chains)
  }
  implicit val decoder: Decoder[MarkovForrest] = Decoder[Map[PositiveInt, MarkovChain]].map(MarkovForrest(_))

  private type MapOfWordCounts = Map[String, PositiveInt]

  private def buildLevel(rawLevel: Map[State, MapOfWordCounts]): Option[MarkovChain] =
    NonEmptyMap.fromMap {
      SortedMap[State, Alternatives](
        rawLevel
          .toSeq
          .flatMap {
            case (key, mapOfWordCounts) =>
              NonEmptyVector
                .fromVector(mapOfWordCounts.map((Choice(_, _)).tupled).toVector)
                .map(_.sorted)
                .map(Alternatives(_))
                .map(key -> _)
          }: _*
      )
    }.map(MarkovChain(_))

  private def incrementCountForWord(word: String, keyBuffer: FIFO[String],
                                    wordCounts: Map[State, MapOfWordCounts]): Map[State, MapOfWordCounts] =
    NonEmptyVector
      .fromVector(keyBuffer.buffer)
      .map(State(_))
      .fold(wordCounts) { key =>
        wordCounts.setOrCompute(key, Map(word -> PositiveInt.One)) {
          _.setOrCompute(word, PositiveInt.One)(_.next)
        }
      }

  /**
    * Build hierarchy of Markov chains from the entities in `source` by counting the occurrences of
    * a bunch of parallel sliding windows of size from 1 to `maxDepth`
    *
    * @param maxLag Maximum window size, this will be the default number of words used as the current state
    * @param source A stream of strings
    * @tparam F The wrapping type
    */
  def from[F[_] : Functor](maxLag: PositiveInt, source: Stream[F, String])
                          (implicit CO: Stream.Compiler[F, F]): F[MarkovForrest] = {
    val initialBuffersAndCounts: NonEmptyVector[(FIFO[String], Map[State, MapOfWordCounts])] =
      (PositiveInt.One to maxLag)
        .toNEV
        .map(FIFO.init[String](_) -> Map.empty[State, MapOfWordCounts])

    source
      .fold(initialBuffersAndCounts) {
        case (oldBuffersAndCounts, word) =>
          oldBuffersAndCounts.map {
            case (keyBuffer, level) if keyBuffer.nonFull => keyBuffer.insert(word) -> level
            case (keyBuffer, level)                      =>
              keyBuffer.insert(word) -> incrementCountForWord(word, keyBuffer, level)
          }
      }
      .map(_.map(_._2))
      .map { wordCountByLevel =>
        MarkovForrest({
          for {
            wordCounts <- wordCountByLevel.toVector
            level <- buildLevel(wordCounts)
            keyLen <- level.keyLength
          } yield keyLen -> level
        }.toMap)
      }
      .compile[F, F, MarkovForrest]
      .foldMonoid
  }
}