package peschke.markov.utils

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.instances.int._
import cats.kernel.Hash
import cats.syntax.applicative._
import cats.syntax.option._
import cats.syntax.either._
import cats.{Eq, NonEmptyTraverse, Order, Semigroup, Show}
import com.monovore.decline.Argument
import io.circe.{Decoder, Encoder, Json, KeyDecoder, KeyEncoder}
import peschke.markov
import peschke.markov.utils

import scala.language.higherKinds

/**
  * Allows the creation and propagation of values known to be positive integers
  */
sealed abstract class PositiveInt {
  /**
    * @return the wrapped value
    */
  def value: Int

  /**
    * @return `value - 1`, if that would be positive, [[scala.None]] if it would be 0 or negative
    */
  def previous: Option[PositiveInt] = PositiveInt.valueOf(value - 1)

  /**
    * @return `value + 1`
    */
  def next: PositiveInt = new PositiveInt.HiddenImpl(value + 1)

  /**
    * Create an inclusive range
    */
  def to(inclusiveEnd: PositiveInt): PositiveRange = PositiveRange.between(this, inclusiveEnd)

  /**
    * Basic addition
    */
  def +(other: PositiveInt): PositiveInt = new utils.PositiveInt.HiddenImpl(value + other.value)

  /**
    * Sort of subtraction, but not exactly.
    * Calculates the distance between two values.
    *
    * @return if `this` and `other` are the same: return [[scala.None]].
    *         otherwise: `|a-b|`
    */
  def distance(other: PositiveInt): Option[PositiveInt] =
    if (value == other.value) none
    else new markov.utils.PositiveInt.HiddenImpl((value - other.value).abs).some

  /**
    * Overridden because it's required by the contract of [[java.lang.Object]]
    *
    * Conveniently, [[cats.Hash]] extends [[cats.Eq]], so we can be sure they're consistent.
    */
  override def equals(obj: Any): Boolean = obj match {
    case pi: PositiveInt => Eq[PositiveInt].eqv(pi, this)
    case _               => false
  }

  /**
    * Overridden so this can be used as a hash key in [[peschke.markov.MarkovForrest]]
    *
    * Using [[cats.Hash]] because it makes hashing easy.
    */
  override def hashCode(): Int = Hash[PositiveInt].hash(this)

  override def toString: String = s"PositiveInt($value)"
}

/**
  * These have lower priority because [[cats.Order]] and [[cats.Hash]] both extend [[cats.Eq]], and this
  * confuses the compiler.
  */
trait PositiveIntInstances1 {
  implicit val order: Order[PositiveInt] = Order.by(_.value)
  implicit val ordering: Ordering[PositiveInt] = order.toOrdering
}

trait PositiveIntInstances0 extends PositiveIntInstances1 {
  // Cats Instances
  implicit val hash: Hash[PositiveInt] = Hash.by(_.value)
  implicit val semigroup: Semigroup[PositiveInt] = Semigroup.instance(_ + _)
  implicit val show: Show[PositiveInt] = Show.show(_.value.toString)

  // Decline Instance
  implicit val argument: Argument[PositiveInt] = new Argument[PositiveInt] {
    override def read(string: String): ValidatedNel[String, PositiveInt] =
      Argument.readInt.read(string).andThen {
        (PositiveInt.valueOf(_: Int)) andThen (Validated.fromOption(_, "must be a positive integer".pure[NonEmptyList]))
      }

    override def defaultMetavar: String = "Z+"
  }

  // Circe Instances
  implicit val encoder: Encoder[PositiveInt] = Encoder.instance(pi => Json.fromInt(pi.value))
  implicit val decoder: Decoder[PositiveInt] = Decoder.decodeInt.emap { i =>
    Either.fromOption(PositiveInt.valueOf(i), "PositiveInt")
  }

  implicit val keyEncoder: KeyEncoder[PositiveInt] = KeyEncoder.instance(Show[PositiveInt].show)
  implicit val keyDecoder: KeyDecoder[PositiveInt] = KeyDecoder.instance(PositiveInt.valueOf)
}

object PositiveInt extends PositiveIntInstances0 {
  def valueOf(unchecked: Int): Option[PositiveInt] =
    if (unchecked <= 0) None
    else Some(new HiddenImpl(unchecked))

  def valueOf(raw: String): Option[PositiveInt] =
    Either.catchOnly[NumberFormatException](raw.toInt).toOption.flatMap(PositiveInt.valueOf)

  /**
    * Get the size of one of the Non-Empty data structures
    *
    * We can return [[peschke.markov.utils.PositiveInt]] directly here because the
    * [[cats.NonEmptyTraverse]] constraint guarantees `net` will have a size of at
    * least 1
    */
  def lengthOf[A[_] : NonEmptyTraverse, B](net: A[B]): PositiveInt =
    new HiddenImpl(NonEmptyTraverse[A].size(net).toInt)

  private[PositiveInt] class HiddenImpl(val value: Int) extends PositiveInt

  // A few commonly used sizes
  val One: PositiveInt = new HiddenImpl(1)
  val Two: PositiveInt = new HiddenImpl(2)
  val Three: PositiveInt = new HiddenImpl(3)
  val Five: PositiveInt = new HiddenImpl(5)
  val Ten: PositiveInt = new HiddenImpl(10)
  val Twenty: PositiveInt = new HiddenImpl(20)
}