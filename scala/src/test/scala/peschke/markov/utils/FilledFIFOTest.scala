package peschke.markov.utils

import org.scalatest.{MustMatchers, WordSpec}

class FilledFIFOTest extends WordSpec with MustMatchers {
  val Width: PositiveInt = PositiveInt.One.next.next

  "FilledFIFO.insert" should {
    "not grow the size of the buffer" in {
      val v0 = FilledFIFO(1, 2, 3)

      val v1 = v0.insert(4)
      v1 mustBe FilledFIFO(2, 3, 4)

      val v2 = v1.insert(5)
      v2 mustBe FilledFIFO(3, 4, 5)

      val v3 = v2.insert(6)
      v3 mustBe FilledFIFO(4, 5, 6)
    }
  }

  "FilledFIFO.shrink" should {
    "stop when the size would be invalid" in {
      FilledFIFO(1).shrink mustBe None
    }

    "discard using FIFO semantics" in {
      FilledFIFO(4, 5, 6, 7).shrink mustBe Some(FilledFIFO(5, 6, 7))
    }
  }
}
