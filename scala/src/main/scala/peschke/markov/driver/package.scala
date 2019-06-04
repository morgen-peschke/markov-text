package peschke.markov

import cats.Semigroup
import cats.effect.ExitCode

package object driver {
  implicit val exitCodeSemigroup: Semigroup[ExitCode] = Semigroup.instance {
    case (ExitCode.Success, ExitCode.Success) => ExitCode.Success
    case (ExitCode.Success, rhs)              => rhs
    case (lhs, _)                             => lhs
  }
}
