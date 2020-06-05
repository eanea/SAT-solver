object CNF {

  trait CNF[A] {
    def cnf(f: A): (CNForm, Literal[Theory])
  }

  implicit def NNFtoCNF: CNF[NNForm] = (f: NNForm) => {
    def go(formula: NNForm): (Seq[Conjunct[Disjunct[Literal[Theory]]]], Literal[Theory]) = {
      formula match {
        case NnfConjunct(conj) =>
          val (r, s) = conj.map(p => go(p)).unzip
          val form = Conjunct(s)
          val rval: Literal[Theory] = Atom(Theory.rval)
          val eq = rval |<=> form
          (r.flatten :+ eq, rval)
        case NnfDisjunct(disj) =>
          val (r, s) = disj.map(p => go(p)).unzip
          val form = Disjunct(s)
          val rval: Literal[Theory] = Atom(Theory.rval)
          val eq = rval |<=> form
          (r.flatten :+ eq, rval)
        case literal: NnfLiteral[_] => (Seq.empty, literal.unlift)
      }
    }

    val (r, phi) = go(f)
    (CNForm(r.tail.foldLeft(r.head)((acc, R) => R && acc)), phi)
  }
}
