import cats.effect.IO

sealed trait ClauseStatus
final case class Updated(add: Literal[Theory], w: Watcher) extends ClauseStatus
final case class UnitClause(lit: Literal[Theory])          extends ClauseStatus
case object TrueClause                                     extends ClauseStatus

case object Backtrackx

final case class Trace(M: Vector[Literal[Theory]] = Vector.empty, lastDecide: List[Int] = Nil) {
  def addDecide(atom: Literal[Theory]): Trace =
    Trace(M :+ atom, M.length :: lastDecide)
  def addUnitProp(atom: Literal[Theory]): Trace = Trace(M :+ atom, lastDecide)

  def lastOption = M.lastOption

  def last = M.last

  def backtrack(activeRing: ActiveRing): Either[Fail.type, (Trace, ActiveRing)] =
    if (lastDecide.nonEmpty) {
      val prefix = M.slice(0, lastDecide.head) :+ !M(lastDecide.head)
      val suffix = M.slice(lastDecide.head + 1, M.length)
      Right(
        Trace(
          prefix,
          lastDecide.tail
        ),
        ActiveRing(activeRing.ring ++ suffix.map(_.makeAtom))
      )
    } else Left(Fail)
}

final case class ActiveRing(ring: Vector[Atom[Theory]]) extends AnyVal {
  def nextEither(trace: Trace): Either[String, Atom[Theory]] = {
    if (ring.isEmpty) Left("ActiveRing nextEither isEmpty")
    else
      Right(ring.head)
  }
}

/**
  * @param disjId
  * @param litPos is 0 or 1
  */
final case class Watcher(disjId: Int, litPos: Int) {
  def moveWatcher(conj: CNForm, trace: Trace): (CNForm, ClauseStatus) = {
    val disj: Disjunct[Literal[Theory]] = conj.cnf.conj(disjId)
    val traceSet                        = trace.M.toSet

    val isTrueClause = disj.disj.exists(l => traceSet(!l))

    if (isTrueClause) {
      (conj, TrueClause)
    } else {
      val unassignedLit =
        disj.disj.zipWithIndex
          .drop(2)
          .filterNot(l => traceSet(l._1))
      if (unassignedLit.isEmpty) {
        (conj, UnitClause(disj.disj(neighbourId)))
      } else {
        val next = unassignedLit.head
        val newDisj =
          disj.disj.updated(litPos, next._1).updated(disj.disj.indexOf(next._1), disj.disj(litPos))
        (
          CNForm(Conjunct(conj.cnf.conj.updated(disjId, Disjunct(newDisj)))),
          Updated(next._1, this)
        )

      }
    }
  }

  def neighbourId: Int = Math.abs(litPos - 1)
}

final case class Watchers(watchersMap: Map[Literal[Theory], Set[Watcher]]) extends AnyVal {
  def updateWatch(
      conj: CNForm,
      v: Literal[Theory],
      trace: Trace
  ): (CNForm, Trace, Watchers, Vector[UnitClause]) = {
    watchersMap.get(v) match {
      case Some(value) =>
        val (cnfff, wm, clsList) = value
          .foldLeft[
            (CNForm, Map[Literal[Theory], Set[Watcher]], Vector[UnitClause])
          ]((conj, watchersMap, Vector.empty))((acc, wz) => {
            val (nextCNF, status) = wz.moveWatcher(acc._1, trace)
            status match {
              case Updated(add, ww) => {
                val deletedList = acc._2(v) - ww
                val updatedMap = acc._2
                  .updated(v, deletedList)
                  .updated(add, acc._2.getOrElse(add, Set.empty) + ww)
                (nextCNF, updatedMap, acc._3)
              }
              case s @ UnitClause(lit) =>
                (nextCNF, acc._2, acc._3 :+ s)
              case TrueClause => (acc._1, acc._2, acc._3)
            }

          })
        (cnfff, trace, Watchers(wm), clsList)
      case None => {
        (conj, trace, this, Vector.empty)
      }
    }
  }
}

final case class DPLL(cnf: CNForm, trace: Trace, watchers: Watchers, ring: ActiveRing)

trait DPLLStatus
case object Fail                 extends DPLLStatus
case object Conflict             extends DPLLStatus
final case class SAT(dpll: DPLL) extends DPLLStatus

object DPLL {

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

  def unitOrDecide(
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

  def update(dpll: DPLL, isStart: Boolean): IO[(DPLL, Vector[UnitClause])] = {
    if (!isStart) updateWatchersByLastLit(dpll)
    else IO(dpll, Vector.empty)
  }

  def updateWatchersByLastLit(
      dpll: DPLL,
  ): IO[(DPLL, Vector[UnitClause])] = IO {
    val lit = dpll.trace.last
    val res = dpll.watchers.updateWatch(dpll.cnf, lit, dpll.trace)
    (dpll.copy(cnf = res._1, watchers = res._3), res._4)
  }

  def unitPropagate(
      dpll: DPLL,
      unitClause: UnitClause
  ): IO[Either[Backtrack.type, DPLL]] = IO {
    val lit: Literal[Theory] = !unitClause.lit
    if (dpll.trace.M.contains(unitClause.lit)) Left(Backtrack)
    else
      Right(
        dpll
          .copy(trace = dpll.trace.addUnitProp(lit), ring = ActiveRing((dpll.ring.ring.toSet - lit.makeAtom).toVector))
      )
  }

  def backtrack(dpll: DPLL): IO[Either[Fail.type, DPLL]] = IO {
    dpll.trace.backtrack(dpll.ring) match {
      case Left(value) =>
        Left(Fail)
      case Right((revertedTrace, revertedRing)) =>
        Right(dpll.copy(trace = revertedTrace, ring = revertedRing))
    }
  }

  def decide(dpll: DPLL): IO[Either[DPLL, SAT]] = IO {
    dpll.ring.nextEither(trace = dpll.trace) match {
      case Left(value) =>
        Right(SAT(dpll))
      case Right(lit) =>
        Left(dpll.copy(trace = dpll.trace.addDecide(lit), ring = ActiveRing((dpll.ring.ring.toSet - lit).toVector)))
    }
  }

  def initWatchers(conjunct: Conjunct[Disjunct[Literal[Theory]]]) =
    conjunct.conj.zipWithIndex.foldLeft(Map[Literal[Theory], Set[Watcher]]()) {
      case (acc, (disj, i)) => {
        val litToWatcher = disj.disj.take(2).zipWithIndex.map(t => (t._1, Watcher(i, t._2)))
        litToWatcher.foldLeft(acc)((acc2, p) => acc2.updated(p._1, acc2.getOrElse(p._1, Set()) + p._2))
      }
    }
}
