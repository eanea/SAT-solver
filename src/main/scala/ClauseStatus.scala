sealed trait ClauseStatus
final case class Updated(add: Literal[Theory], w: Watcher) extends ClauseStatus
final case class UnitClause(lit: Literal[Theory])          extends ClauseStatus
case object TrueClause                                     extends ClauseStatus