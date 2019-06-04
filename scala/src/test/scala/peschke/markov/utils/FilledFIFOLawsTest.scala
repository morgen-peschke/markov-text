package peschke.markov.utils

import cats.data.NonEmptyVector
import cats.laws.discipline.MonadTests
import cats.tests.CatsSuite
import org.scalacheck.{Arbitrary, Gen}

class FilledFIFOLawsTest extends CatsSuite {
  implicit def arbFifo[A: Arbitrary]: Arbitrary[FilledFIFO[A]] =
    Arbitrary(
      for {
        tailLength <- Gen.chooseNum(0, 30)
        first <- Arbitrary.arbitrary[A]
        rest <- Gen.containerOfN[Vector, A](tailLength, Arbitrary.arbitrary[A])
      } yield FilledFIFO(NonEmptyVector(first, rest))
    )

  checkAll("FilledFIFO", MonadTests[FilledFIFO].monad[Int, String, Int])
}
