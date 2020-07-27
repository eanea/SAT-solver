final case class Trace(M: Vector[Literal[Theory]] = Vector.empty, lastDecide: List[Int] = Nil) {
  def addDecide(atom: Literal[Theory]): Trace =
    Trace(M :+ atom, M.length :: lastDecide)
  def addUnitProp(atom: Literal[Theory]): Trace = Trace(M :+ atom, lastDecide)

  def lastOption: Option[Literal[Theory]] = M.lastOption

  def last: Literal[Theory] = M.last

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