import cats.effect.{IO, Resource}

import scala.io.BufferedSource
final case class DPLL(cnf: CNForm, trace: Trace, watchers: Watchers, ring: ActiveRing)
object DPLL {

  def fromDimacs(filename: String): IO[Vector[Map[String, Boolean]]] = {
    val file = IO(scala.io.Source.fromFile(filename))

    for {
      (cnfForm, ring) <- parseDimacs(file)
      watchers        <- createWatchers(cnfForm)
      dpll            <- createDPLL(cnfForm, Trace(), watchers, ring)
      satRes          <- DPLL.solve(dpll, Vector.empty, 0, Vector.empty, isStart = true)
      res             <- makeHumanReadeable(satRes)
    } yield res
  }

  private def makeHumanReadeable(vt: Vector[Trace]): IO[Vector[Map[String, Boolean]]] = IO {
    vt.foldLeft(Vector.empty[Map[String, Boolean]]) { (acc, t) => acc :+ traceToMap(t) }
  }

  private def traceToMap(t: Trace): Map[String, Boolean] = t.M.foldLeft(Map.empty[String, Boolean]) { (acc, lit) =>
    lit match {
      case Atom(a) =>
        acc + {
          a match {
            case Rvar(id)  => (id.toString, false)
            case Var(name) => (name, false)
          }
        }
      case NotAtom(a) =>
        acc + {
          a match {
            case Rvar(id)  => (id.toString, true)
            case Var(name) => (name, true)
          }
        }
    }
  }

  private def createDPLL(cnf: CNForm, trace: Trace, watchers: Watchers, ring: ActiveRing): IO[DPLL] = IO {
    DPLL(cnf, trace, watchers, ring)
  }

  private def createWatchers(CNForm: CNForm): IO[Watchers] = IO {
    Watchers(initWatchers(CNForm.cnf))
  }

  private def parseDimacs(fileBuf: IO[BufferedSource]): IO[(CNForm, ActiveRing)] = {
    Resource.fromAutoCloseable(fileBuf).use { buf: BufferedSource =>
      IO {
        val disjSetPair = for {
          line <- buf.getLines
          if !line.startsWith("c") && !line.startsWith("p")
        } yield {
          parseLine(line)
        }

        val (disjs, atoms) = disjSetPair.foldLeft((List.empty[Disjunct[Literal[Theory]]], Set.empty[Atom[Theory]])) {
          (acc, p) => (p._1 :: acc._1, acc._2 ++ p._2)
        }

        (CNForm(Conjunct(disjs)), ActiveRing(atoms.toVector))
      }
    }
  }

  private def parseLine(line: String): (Disjunct[Literal[Theory]], Set[Atom[Theory]]) = {
    val (literals, atoms) = line
      .split(" ")
      .dropRight(1)
      .map(_.toInt)
      .map { x =>
        if (x > 0) {
          val lit  = Theory.atom(s"$x")
          val atom = Atom(Var(s"$x"))
          (lit, atom)
        } else {
          val lit  = Theory.notAtom(s"${-x}")
          val atom = Atom(Var(s"${-x}"))
          (lit, atom)
        }
      }
      .unzip

    Disjunct(literals) -> Set.from(atoms)
  }

  def solve(
      dpll: DPLL,
      unitClausesList: Vector[UnitClause],
      iter: Int,
      agg: Vector[Trace],
      isStart: Boolean = false
  ): IO[Vector[Trace]] = {
    for {
      (updateWatchDpll, unitClausesOrEmpty) <- update(dpll, isStart)
      nextUnitClausesList                   = unitClausesList ++ unitClausesOrEmpty
      hasUnitClause                         = nextUnitClausesList.nonEmpty
      res                                   <- unitOrDecide(updateWatchDpll, nextUnitClausesList, agg, iter, hasUnitClause)
    } yield res
  }

  private def unitOrDecide(
      updateWatchDpll: DPLL,
      nextUnitClausesList: Vector[UnitClause],
      agg: Vector[Trace],
      iter: Int,
      hasUnitClause: Boolean
  ): IO[Vector[Trace]] = {
    if (hasUnitClause) unitPropagate(updateWatchDpll, nextUnitClausesList.head).flatMap {
      case Left(value) =>
        backtrack(updateWatchDpll).flatMap {
          case Left(value) => IO.pure(agg)
          case Right(backtrackedDPLL) =>
            solve(backtrackedDPLL, Vector.empty, iter + 1, agg)
        }
      case Right(unitPropagateDpll) =>
        solve(unitPropagateDpll, nextUnitClausesList.tail, iter + 1, agg)
    }
    else
      decide(updateWatchDpll).flatMap {
        case Left(nextDpll) =>
          solve(nextDpll, Vector.empty, iter + 1, agg)
        case Right(sat) => {
          backtrack(sat.dpll).flatMap {
            case Left(value) => IO.pure(agg)
            case Right(decideBacktrackDpll) =>
              solve(decideBacktrackDpll, Vector.empty, iter + 1, agg :+ sat.dpll.trace)
          }
        }
      }
  }

  private def update(dpll: DPLL, isStart: Boolean): IO[(DPLL, Vector[UnitClause])] = {
    if (!isStart) updateWatchersByLastLit(dpll)
    else IO(dpll, Vector.empty)
  }

  private def updateWatchersByLastLit(dpll: DPLL): IO[(DPLL, Vector[UnitClause])] = IO {
    val lit = dpll.trace.last
    val res = dpll.watchers.updateWatch(dpll.cnf, lit, dpll.trace)
    (dpll.copy(cnf = res._1, watchers = res._3), res._4)
  }

  private def unitPropagate(dpll: DPLL, unitClause: UnitClause): IO[Either[Backtrack.type, DPLL]] = IO {
    val lit: Literal[Theory] = !unitClause.lit
    if (dpll.trace.M.contains(unitClause.lit)) Left(Backtrack)
    else
      Right(
        dpll
          .copy(trace = dpll.trace.addUnitProp(lit), ring = ActiveRing((dpll.ring.ring.toSet - lit.makeAtom).toVector))
      )
  }

  private def backtrack(dpll: DPLL): IO[Either[Fail.type, DPLL]] = IO {
    dpll.trace.backtrack(dpll.ring) match {
      case Left(value) =>
        Left(Fail)
      case Right((revertedTrace, revertedRing)) =>
        Right(dpll.copy(trace = revertedTrace, ring = revertedRing))
    }
  }

  private def decide(dpll: DPLL): IO[Either[DPLL, SAT]] = IO {
    dpll.ring.nextEither(trace = dpll.trace) match {
      case Left(value) =>
        Right(SAT(dpll))
      case Right(lit) =>
        Left(dpll.copy(trace = dpll.trace.addDecide(lit), ring = ActiveRing((dpll.ring.ring.toSet - lit).toVector)))
    }
  }

  def initWatchers(conjunct: Conjunct[Disjunct[Literal[Theory]]]): Map[Literal[Theory], Set[Watcher]] =
    conjunct.conj.zipWithIndex.foldLeft(Map[Literal[Theory], Set[Watcher]]()) {
      case (acc, (disj, i)) => {
        val litToWatcher = disj.disj.take(2).zipWithIndex.map(t => (t._1, Watcher(i, t._2)))
        litToWatcher.foldLeft(acc)((acc2, p) => acc2.updated(p._1, acc2.getOrElse(p._1, Set()) + p._2))
      }
    }
}
