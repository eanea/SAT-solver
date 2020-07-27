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