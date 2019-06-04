package peschke.markov

import cats.data.{NonEmptyList, NonEmptyVector}
import cats.syntax.option._
import cats.syntax.foldable._
import cats.instances.vector._
import cats.kernel.Order
import peschke.markov.utils.{FilledFIFO, PositiveInt, RandomNumberGenerator, Sentence}

import scala.annotation.tailrec

case class SentenceGenerator(forrest: MarkovForrest) {
  /**
    * Chains together multiple invocations of [[peschke.markov.SentenceGenerator#nextWord]], in an attempt to
    * generate a sentence of length `targetLength`.
    *
    * Emits the sentence, if created, and the most recent value of the RNG seed
    */
  def generate[S: RandomNumberGenerator](targetLength: PositiveInt, rngSeed: S): (S, Option[Sentence]) = {
    def stepN(statePrev: FilledFIFO[String], seedPrev: S): ((Option[String], FilledFIFO[String]), S) = {
      val (seedN, wordNOpt) = nextWord(statePrev, seedPrev)
      wordNOpt -> wordNOpt.fold(statePrev)(statePrev.insert) -> seedN
    }

    forrest
      .chains
      .toVector
      // This bit is picking the sentence anchor, which we have to do explicitly
      // because the Markov chain needs to have some initial state
      .maximumOption(Order.by[(PositiveInt, MarkovChain), PositiveInt](_._1))
      .map {
        case (_, chain) =>
          val keyNes = chain.transitions.keys
          val keyNev = NonEmptyVector(keyNes.head, keyNes.tail.toVector)
          val length = PositiveInt.lengthOf(keyNev)
          val (seed0, keyIndex) = RandomNumberGenerator[S].next(rngSeed, length)
          seed0 -> FilledFIFO(keyNev.getUnsafe(keyIndex.value - 1).parts)
      }
      .map {
        case (seed0, state0) =>
          val (seeds, maybeWords) =
            Iterator
              .iterate(stepN(state0, seed0)) {
                case ((_, statePrev), seedPrev) => stepN(statePrev, seedPrev)
              }
              .takeWhile(_._1._1.isDefined)
              .take(targetLength.value - state0.buffer.length)
              .toVector
              .map {
                case ((wordOpt, _), seed) => seed -> wordOpt
              }
              .unzip

          val resultingSeed = seeds.lastOption.getOrElse(rngSeed)
          val sentenceOpt = NonEmptyList.fromList(state0.buffer.toList ++ maybeWords.flatten.toList).map(Sentence(_))
          (resultingSeed, sentenceOpt)
      }
      .getOrElse(rngSeed -> none)
  }

  /**
    * Attempts to generate a word, based on the current state.
    *
    * To do this, it starts at the highest width chain and only shrinks when nothing is found, and only as far as
    * necessary.
    */
  def nextWord[S](state: FilledFIFO[String], rngSeed: S)
                 (implicit R: RandomNumberGenerator[S]): (S, Option[String]) = {
    @tailrec
    def getChainOpt(currentState: FilledFIFO[String]): Option[(FilledFIFO[String], MarkovChain)] =
      forrest.chains.get(currentState.capacity) match {
        case Some(chain) => Some(currentState -> chain)
        case None        => currentState.shrink match {
          case Some(smallerState) => getChainOpt(smallerState)
          case None               => none
        }
      }

    def choose(alternatives: Alternatives, currentSeed: S): (S, Option[String]) = {
      val (nextSeed, randomWeight) = R.next(currentSeed, alternatives.totalWeight)

      @tailrec
      def loop(remaining: Vector[Choice], weight: Int): Option[String] = {
        remaining match {
          case Vector()                                 => none
          case Vector(singleChoice)                     => singleChoice.word.some
          case head +: _ if weight <= head.weight.value => head.word.some
          case head +: tail                             => loop(tail, weight - head.weight.value)
        }
      }

      (nextSeed, loop(alternatives.alternatives.toVector, randomWeight.value))
    }

    @tailrec
    def attemptState(currentState: FilledFIFO[String], currentSeed: S): (S, Option[String]) =
      getChainOpt(currentState) match {
        case Some((foundWithState, chain)) =>
          chain.transitions.lookup(State(foundWithState.buffer)) match {
            case Some(choices) => choose(choices, currentSeed)

            case None => foundWithState.shrink match {
              case Some(smallerState) => attemptState(smallerState, currentSeed)
              case None               => (currentSeed, none)
            }
          }
        case None                          => currentState.shrink match {
          case Some(smallerState) => attemptState(smallerState, currentSeed)
          case None               => (currentSeed, none)
        }
      }

    attemptState(state, rngSeed)
  }
}
