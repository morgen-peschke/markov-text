package peschke.markov.utils

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

/**
  * Helper class which exists to make writing the [[io.circe.Encoder]] and
  * [[io.circe.Decoder]] easier for [[peschke.markov.MarkovChain]].
  */
case class KeyAndValue[K, V](key: K, value: V) {
  def toTuple: (K, V) = key -> value
}
object KeyAndValue {
  def fromTuple[K, V](t: (K, V)): KeyAndValue[K, V] = KeyAndValue(t._1, t._2)

  implicit def encoder[K: Encoder, V: Encoder]: Encoder[KeyAndValue[K, V]] = deriveEncoder
  implicit def decoder[K: Decoder, V: Decoder]: Decoder[KeyAndValue[K, V]] = deriveDecoder
}