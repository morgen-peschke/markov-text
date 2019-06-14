package peschke.markov.driver

import java.nio.charset.Charset
import java.nio.file.{Files, Path}
import java.util.concurrent.Executors

import cats.effect.{ContextShift, ExitCode, IO, Resource}
import cats.syntax.semigroup._
import io.circe
import fs2.{Stream, io, text}
import peschke.markov.MarkovForrest
import peschke.markov.utils.{PositiveInt, Sentence}

import scala.concurrent.ExecutionContext

object FileIO {
  private val BufferSize = 2048

  private def makeBlockingExecutionContext =
    Resource
      .make(IO(ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(2))))(ec => IO(ec.shutdown()))

  private def streamContents(source: Option[Path], ec: ExecutionContext)(implicit cs: ContextShift[IO]) =
    source.fold(io.stdin[IO](BufferSize, ec))(io.file.readAll[IO](_, ec, BufferSize))
      .through(text.utf8Decode)

  /**
    * Write a serialized version of `forrest`, to make it easier to
    * do subsequent runs using the same forrest.
    */
  def saveModelToPath(dest: Path, forrest: MarkovForrest): IO[ExitCode] = {
    val createFile = IO {
      if (Files.notExists(dest)) {
        Files.createFile(dest)
      }
      if (Files.exists(dest)) ExitCode.Success else ExitCode(68)
    }

    val buildJson = IO {
      import circe.syntax._
      forrest.asJson.noSpaces
    }

    def writeJson(jsonString: String) =
      Resource.fromAutoCloseable(IO(Files.newBufferedWriter(dest, Charset.forName("UTF-8"))))
        .use(bufferedWriter => IO {
          bufferedWriter.write(jsonString)
          ExitCode.Success
        })

    for {
      cfr <- createFile
      json <- buildJson
      wjr <- writeJson(json)
    } yield cfr.combine(wjr)
  }

  /**
    * Load the input from the given path (or STDIN if omitted), and
    * build a [[peschke.markov.MarkovForrest]] from it.
    */
  def loadInputAsRawText(source: Option[Path], lag: PositiveInt)(implicit cs: ContextShift[IO]): IO[MarkovForrest] =
    MarkovForrest.from[IO](lag,
      for {
        blockingEC <- Stream.resource(makeBlockingExecutionContext)
        line <- streamContents(source, blockingEC).through(text.lines)
        word <- Sentence.from(line).fold[Stream[IO, String]](Stream.empty)(_.stream)
      } yield word
    )
}
