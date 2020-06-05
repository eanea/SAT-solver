import org.scalactic._
import TypeCheckedTripleEquals._
import cats.effect.{ExitCode, IO, IOApp}
import DPLL.{solve, initWatchers}

object UNSAT1 extends IOApp {
  val ax1 = Atom(Var("1"))
  val ax2 = Atom(Var("2"))

  val x1: Literal[Theory] = Atom(Var("1"))
  val x2: Literal[Theory] = Atom(Var("2"))

  val c1 = x1 || x2
  val c2 = !x1 || x2
  val c3 = x1 || !x2
  val c4 = !x1 || !x2

  val conj: Conjunct[Disjunct[Literal[Theory]]] = Conjunct(Seq(c1, c2, c3, c4))

  val activeRingStart = ActiveRing(Vector(ax1, ax2))

  override def run(args: List[String]): IO[ExitCode] = {
    val dpllStart = DPLL(CNForm(conj), Trace(), Watchers(initWatchers(conj)), activeRingStart)

    for {
      agg <- solve(dpllStart, Vector.empty, 0, Vector.empty, isStart = true)
      isTestPassed <- IO.pure(agg === Vector.empty)
      _ <- IO.pure{
        println(s"Test passed = $isTestPassed")
      }
    } yield ExitCode.Success
  }

}
