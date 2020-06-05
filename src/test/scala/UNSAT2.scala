import DPLL.{initWatchers, solve}
import cats.effect.{ExitCode, IO, IOApp}
import org.scalactic.TypeCheckedTripleEquals._
import org.scalactic._

object UNSAT2 extends IOApp {
  val ax1 = Atom(Var("1"))
  val ax2 = Atom(Var("2"))
  val ax3 = Atom(Var("3"))
  val ax4 = Atom(Var("4"))
  val ax5 = Atom(Var("5"))
  val ax6 = Atom(Var("6"))

  val x1: Literal[Theory] = Atom(Var("1"))
  val x2: Literal[Theory] = Atom(Var("2"))
  val x3: Literal[Theory] = Atom(Var("3"))
  val x4: Literal[Theory] = Atom(Var("4"))
  val x5: Literal[Theory] = Atom(Var("5"))
  val x6: Literal[Theory] = Atom(Var("6"))

    val c1                                        = !x1 || x3 || x4
    val c2                                        = !x2 || x6 || x4
    val c3                                        = !x2 || !x6 || !x3
    val c4                                        = !x4 || !x2
    val c5                                        = x2 || !x3 || !x1
    val c6                                        = x2 || x6 || x3
    val c7                                        = x2 || !x6 || !x4
    val c8                                        = x1 || x5
    val c9                                        = x1 || x6
    val c10                                       = !x6 || x3 || !x5
    val c11                                       = x1 || !x3 || !x5
    val conj: Conjunct[Disjunct[Literal[Theory]]] = Conjunct(Seq(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11))
  val activeRingStart = ActiveRing(Vector(ax1, ax2, ax3, ax4, ax5, ax6))

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
