package peschke.markov.driver

import java.nio.file.Path

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.applicative._
import cats.syntax.order._
import cats.syntax.show._
import peschke.markov.info.BuildInfo
import peschke.markov.utils.RandomNumberGenerator.Seed
import peschke.markov.utils.{PositiveInt, PositiveRange, Sentence}
import peschke.markov.{MarkovForrest, SentenceGenerator}


object Main extends IOApp {
  private def generateSeed(overrideSeedOpt: Option[Long]) =
    overrideSeedOpt.fold(IO(Seed(System.currentTimeMillis())))(Seed(_).pure[IO])

  private def generateSentences(generator: SentenceGenerator,
                                rngSeed: Seed,
                                wordsPerSentence: PositiveRange,
                                sentenceCount: PositiveInt): IO[Vector[(Seed, Sentence)]] = IO {
    (PositiveInt.One to sentenceCount).toNEV.foldLeft((rngSeed, Vector.empty[(Seed, Sentence)])) {
      case ((seedCurr, accum), _) =>
        val (seedNext, sentenceOpt) = generator.generate(wordsPerSentence.max, seedCurr)
        (seedNext, accum ++ sentenceOpt.filterNot(_.length < wordsPerSentence.min).map(seedCurr -> _))
    }._2
  }

  private def outputResults(saveModelOpt: Option[Path])
                           (output: Vector[(Seed, Sentence)], forrest: MarkovForrest): IO[ExitCode] = {
    val printResults = IO {
      output.foreach {
        case (seed, sentence) => println(show"$seed|$sentence")
      }
    }
    for {
      _ <- printResults
      ec <- saveModelOpt.fold(ExitCode.Success.pure[IO])(FileIO.saveModelToPath(_, forrest))
    } yield ec
  }

  override def run(args: List[String]): IO[ExitCode] =
    CLIParameters.command.parse(args) match {
      case Left(help) => IO {
        System.err.println(help)
        ExitCode(67)
      }

      case Right(CLIParameters(_, _, _, _, _, _, true)) => IO {
        println(BuildInfo.toJson)
        ExitCode.Success
      }

      case Right(CLIParameters(inputOpt, wordsPerSentenceRange, sentenceCountRange, lag, seedOpt, saveModelOpt, _)) =>
        for {
          forrest <- FileIO.loadInputAsRawText(inputOpt, lag)
          initialSeed <- generateSeed(seedOpt)

          sentencesAndTheirSeeds <- generateSentences(
            SentenceGenerator(forrest),
            initialSeed,
            wordsPerSentenceRange,
            sentenceCountRange)

          exitCode <- outputResults(saveModelOpt)(sentencesAndTheirSeeds, forrest)
        } yield exitCode
    }

}
