package peschke.markov.utils

import cats.{Eq, Monad}
import cats.data.NonEmptyVector
import cats.kernel.Semigroup
import cats.syntax.applicative._
import cats.syntax.eq._
import com.github.ghik.silencer.silent

/**
  * A version of [[peschke.markov.utils.FIFO]] that always operates at maximum capacity.
  *
  * The semantics are largely identical.
  */
final case class FilledFIFO[A](buffer: NonEmptyVector[A]) {
  /**
    * @return The maximum elements in `buffer`
    */
  def capacity: PositiveInt = PositiveInt.lengthOf(buffer)

  /**
    * Reduces the capacity of the FIFO by 1, discarding elements as required.
    *
    * @return A [[peschke.markov.utils.FIFO]] with `capacity - 1` capacity, or [[scala.None]] if that would result
    *         in a [[peschke.markov.utils.FIFO]] which could contain no elements.
    */
  def shrink: Option[FilledFIFO[A]] =
    NonEmptyVector.fromVector(buffer.tail).map(new FilledFIFO[A](_))

  /**
    * Inserts a new value, discarding the oldest value if needed to stay within the capacity of the
    * FIFO
    */
  def insert(value: A): FilledFIFO[A] =
    new FilledFIFO[A](buffer.tail match {
      case Vector()  => NonEmptyVector.one(value)
      case h +: rest => NonEmptyVector(h, rest :+ value)
    })

  @silent private[this] def copy(): FilledFIFO[A] = this
}

object FilledFIFO {
  /**
    * Create a [[peschke.markov.utils.FilledFIFO]], initialized to some initial values.
    */
  def apply[A](a0: A, an: A*): FilledFIFO[A] = new FilledFIFO(NonEmptyVector(a0, an.toVector))

  private val And: Semigroup[Boolean] = Semigroup.instance(_ && _)

  implicit def eq[A: Eq]: Eq[FilledFIFO[A]] = Eq.instance { (a, b) =>
    a.buffer.zipWith(b.buffer)(_ === _).reduce(And)
  }

  implicit val instances: Monad[FilledFIFO] = new Monad[FilledFIFO] {
    override def pure[A](x: A): FilledFIFO[A] = new FilledFIFO(x.pure[NonEmptyVector])

    override def map[A, B](fa: FilledFIFO[A])(f: A => B): FilledFIFO[B] =
      new FilledFIFO(fa.buffer.map(f))

    override def flatMap[A, B](fa: FilledFIFO[A])
                              (f: A => FilledFIFO[B]): FilledFIFO[B] =
      new FilledFIFO(fa.buffer.map(f).flatMap(_.buffer))

    override def tailRecM[A, B](a: A)
                               (f: A => FilledFIFO[Either[A, B]]): FilledFIFO[B] =
      new FilledFIFO(Monad[NonEmptyVector].tailRecM(a)(f andThen (_.buffer)))
  }
}
