object NNF extends App {

  trait NNF[A] {
    def nnf(f: A, b: Boolean): NNForm
  }

  implicit def QFLNNF: NNF[QFL] = new NNF[QFL] {
    def nnf(f: QFL, b: Boolean): NNForm = {
      f match {
        case _ @ Atom(v) if b    => NnfNotAtom(v)
        case a @ Atom(v)         => a.lift
        case _ @ NotAtom(v) if b    => NnfAtom(v)
        case na @ NotAtom(v)         => na.lift
        case Not(x)     => nnf(x, !b)
        case Conjunct(conj) if b => NnfDisjunct(conj.map(nnf(_, b)))
        case Conjunct(conj)      => NnfConjunct(conj.map(nnf(_, b)))
        case Disjunct(disj) if b => NnfConjunct(disj.map(nnf(_, b)))
        case Disjunct(disj)      => NnfDisjunct(disj.map(nnf(_, b)))
      }
    }
  }
}
