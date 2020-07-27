sealed trait DPLLStatus
case object Fail                 extends DPLLStatus
case object Conflict             extends DPLLStatus
final case class SAT(dpll: DPLL) extends DPLLStatus