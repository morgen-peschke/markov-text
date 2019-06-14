package peschke.markov.driver

import java.nio.file.Path

import com.monovore.decline.{Command, Opts}
import cats.syntax.show._
import cats.syntax.apply._
import peschke.markov.info.BuildInfo
import peschke.markov.utils.{PositiveInt, PositiveRange}

case class CLIParameters(input: Option[Path],
                         wordsPerSentence: PositiveRange,
                         sentenceCount: PositiveInt,
                         lag: PositiveInt,
                         seed: Option[Long],
                         saveModel: Option[Path],
                         showInfo: Boolean)
object CLIParameters {
  private val inputArg: Opts[Option[Path]] =
    Opts.argument[Path](metavar = "FILE")
      .orNone
      .map(_.filterNot(_.toString == "-"))

  private val wordsPerSentence = {
    val minWordsPerSentenceDefault = PositiveInt.Ten
    val maxWordsPerSentenceDefault = PositiveInt.Twenty

    val minWordsPerSentenceOption = Opts.option[PositiveInt](
      "min-words",
      help = show"Discard sentences which fail to reach at least this length [default: $minWordsPerSentenceDefault]"
    ).withDefault(minWordsPerSentenceDefault)

    val maxWordsPerSentenceOption = Opts.option[PositiveInt](
      "max-words",
      help = show"Limits sentence length [default: $maxWordsPerSentenceDefault]"
    ).withDefault(maxWordsPerSentenceDefault)

    val rangeOfWordsPerSentenceOption = Opts.option[PositiveRange](
      "words",
      short = "w",
      help = show"Best-effort attempt will be made to generate sentences with word counts in this range " +
             show"[default: $minWordsPerSentenceDefault..$maxWordsPerSentenceDefault]"
    ).withDefault(PositiveRange.between(minWordsPerSentenceDefault, maxWordsPerSentenceDefault))

    (minWordsPerSentenceOption, maxWordsPerSentenceOption)
      .mapN(_ to _)
      .orElse(rangeOfWordsPerSentenceOption)
  }

  private val sentenceCount = {
    val default = PositiveInt.Five

    Opts.option[PositiveInt](
      "count",
      short = "c",
      help = show"Best-effort attempt will be made to generate a this many sentences [default: $default]"
    ).withDefault(default)
  }

  private val lag = {
    val lagDefault = PositiveInt.Three

    Opts.option[PositiveInt](
      "lag",
      short = "l",
      help = show"Set the maximum number of trailing words used as the current state [default: $lagDefault]"
    ).withDefault(lagDefault)
  }

  private val seed = Opts.option[Long](
    "seed",
    short = "s",
    help = "Set the RNG seed"
  ).orNone

  private val saveModel = Opts.option[Path](
    "save-model",
    help = "Save the Markov tree as JSON to a file, mostly useful for debugging. Loading the model turns out to be " +
           "slower than generating it fresh in most cases."
  ).orNone

  private val info: Opts[Boolean] = Opts.flag("info", help = "Output build information as a JSON object and exit")
    .orFalse

  val command: Command[CLIParameters] = Command(
    name = BuildInfo.name,
    header =
      s"""|Version: ${BuildInfo.version}
          |
          |Generate random-ish text from FILE using Markov Chains. If FILE is omitted or - then read from standard
          |input.
          |
          |Sentence lengths referred to below are measured by word count.""".stripMargin,
    helpFlag = true
  )((inputArg, wordsPerSentence, sentenceCount, lag, seed, saveModel, info)
    .mapN(CLIParameters(_, _, _, _, _, _, _)))
}