package peschke.markov.factories

import cats.data.{NonEmptyList, NonEmptyMap, NonEmptyVector}
import peschke.markov.utils.{PositiveInt, Sentence}
import peschke.markov._

import scala.collection.immutable.SortedMap

/**
  * Some factories to make creating values of a particular type easier, which gets really tedious in the tests.
  */
object dsl {

  implicit class ChoiceFromWordDSL(val word: String) extends AnyVal {
    def <#(weight: PositiveInt): Choice = Choice(word, weight)
  }

  def alternatives(p0: Choice, ps: Choice*): Alternatives =
    Alternatives(NonEmptyVector(p0, ps.toVector))

  def chain(l0: (State, Alternatives), ls: (State, Alternatives)*): MarkovChain = MarkovChain(
    NonEmptyMap(l0, SortedMap(ls: _*)))

  def forrest(ps: (PositiveInt, MarkovChain)*): MarkovForrest = MarkovForrest(ps.toMap)

  def sentence(w0: String, ws: String*): Sentence = Sentence(NonEmptyList(w0, ws.toList))
}
