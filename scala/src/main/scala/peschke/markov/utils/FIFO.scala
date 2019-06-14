package peschke.markov.utils

import com.github.ghik.silencer.silent

/**
  * A size-bounded FIFO buffer.
  * @param buffer The contents of the FIFO
  * @param capacity The maximum elements in `buffer`
  */
final case class FIFO[A] private[utils](buffer: Vector[A], capacity: PositiveInt) {
  /**
    * Inserts a new value, discarding the oldest value if needed to stay within the capacity of the
    * FIFO
    */
  def insert(value: A): FIFO[A] = new FIFO(buffer.takeRight(capacity.value - 1) :+ value, capacity)

  /**
    * Reduces the capacity of the FIFO by 1, discarding elements as required.
    *
    * @return A [[peschke.markov.utils.FIFO]] with `capacity - 1` capacity, or [[scala.None]] if that would result
    *         in a [[peschke.markov.utils.FIFO]] which could contain no elements.
    */
  def shrink: Option[FIFO[A]] = capacity.previous.map(FIFO[A](buffer, _))

  def isFull: Boolean = buffer.length == capacity.value

  def nonFull: Boolean = !isFull

  @silent private def copy(): FIFO[A] = this
}
object FIFO {
  /**
    * Create a FIFO, initialized with some initial values.
    *
    * @param buffer The initial contents of the FIFO, if there are more elements than the buffer could hold
    *               the values are discarded from the head.
    * @param capacity the maximum capacity of this FIFO
    */
  def apply[A](buffer: Vector[A], capacity: PositiveInt): FIFO[A] =
    new FIFO[A](buffer.takeRight(capacity.value), capacity)

  /**
    * Create an empty FIFO
    *
    * @param capacity the maximum capacity of this FIFO
    */
  def init[A](capacity: PositiveInt): FIFO[A] = FIFO(Vector.empty[A], capacity)
}
