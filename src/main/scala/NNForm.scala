import CNF.CNF

sealed trait NNForm extends Product with Serializable
object NNForm {
  implicit class RichNNform(f: NNForm) {
    def toCNF(implicit converter: CNF[NNForm]): (CNForm, Literal[Theory]) =
      converter.cnf(f)
  }
}
case class NnfConjunct[A <: NNForm](conj: Seq[A]) extends NNForm
case class NnfDisjunct[A <: NNForm](disj: Seq[A]) extends NNForm
sealed trait NnfLiteral[+A <: Theory] extends NNForm {
  def unlift: Literal[Theory]
}
case class NnfAtom[+A <: Theory](theoryVar: A) extends NnfLiteral[A] {
  def unlift: Atom[A] = Atom(theoryVar)
}
case class NnfNotAtom[+A <: Theory](theoryVar: A) extends NnfLiteral[A] {
  def unlift: NotAtom[A] = NotAtom(theoryVar)
}
