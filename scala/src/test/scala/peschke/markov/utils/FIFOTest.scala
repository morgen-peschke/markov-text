package peschke.markov.utils

import org.scalatest.{MustMatchers, WordSpec}

class FIFOTest extends WordSpec with MustMatchers {
  val Width: PositiveInt = PositiveInt.One.next.next

  "FIFO.apply" should {
    "truncate using FIFO semantics during creation" in {
      FIFO(Vector(1, 2), Width) mustBe new FIFO(Vector(1, 2), Width)
      FIFO(Vector(1, 2, 3), Width) mustBe new FIFO(Vector(1, 2, 3), Width)
      FIFO(Vector(1, 2, 3, 4), Width) mustBe new FIFO(Vector(2, 3, 4), Width)
    }
  }

  "FIFO.insert" should {
    "insert elements until the buffer is full" in {
      val v0 = FIFO.init[Int](Width)
      v0 mustBe FIFO(Vector.empty[Int], Width)

      val v1 = v0.insert(1)
      v1 mustBe FIFO(Vector(1), Width)

      val v2 = v1.insert(2)
      v2 mustBe FIFO(Vector(1, 2), Width)

      val v3 = v2.insert(3)
      v3 mustBe FIFO(Vector(1, 2, 3), Width)
    }

    "stop growing once the buffer is full" in {
      val v0 = FIFO(Vector(1, 2, 3), Width)

      val v1 = v0.insert(4)
      v1 mustBe new FIFO(Vector(2, 3, 4), Width)

      val v2 = v1.insert(5)
      v2 mustBe new FIFO(Vector(3, 4, 5), Width)

      val v3 = v2.insert(6)
      v3 mustBe new FIFO(Vector(4, 5, 6), Width)
    }
  }

  "FIFO.isFull" should {
    "return true only when the buffer size reaches it's maximum size" in {
      FIFO(Vector(5, 6), Width).isFull mustBe false
      FIFO(Vector(4, 5, 6), Width).isFull mustBe true
    }
  }

  "FIFO.shrink" should {
    "stop when the size would be invalid" in {
      FIFO(Vector(1), PositiveInt.One).shrink mustBe None
    }

    "discard using FIFO semantics" in {
      FIFO(Vector(4,5,6,7), Width.next).shrink mustBe Some(FIFO(Vector(5,6,7), Width))
    }
  }
}
