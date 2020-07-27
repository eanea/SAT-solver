import cats.effect.{ExitCode, IO, IOApp}

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = for {
    res <- DPLL.fromDimacs("quinn.cnf")
    _   <- IO(println(res.mkString("\n")))
  } yield ExitCode.Success
}
