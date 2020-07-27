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