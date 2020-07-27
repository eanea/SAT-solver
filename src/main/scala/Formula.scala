import CNF.CNF
import CanBuild._
import NNF.NNF
import cats.Show
import cats.implicits._

sealed trait QFL extends Product with Serializable {
  def &&[A >: this.type <: QFL, B <: QFL, To](that: B)(implicit cb: And[A, B, To] = QFL.and[A, B]): To =
    cb.make(this, that)

  def ||[A >: this.type <: QFL, B <: QFL, To](that: B)(implicit cb: Or[A, B, To] = QFL.or[A, B]): To =
    cb.make(this, that)

  def unary_![A >: this.type <: QFL, To](implicit cb: CanBuildNot[A, To] = QFL.not[A]): To =
    cb.make(this)

  def impl[A >: this.type <: QFL, B <: QFL, To](that: B)(implicit cb: Impl[A, B, To] = QFL.impl[A, B]): To =
    cb.make(this, that)

  def |<=>[A >: this.type <: QFL, B <: QFL, To](that: B)(implicit cb: Eqviv[A, B, To] = QFL.equa[A, B]): To =
    cb.make(this, that)

}

object QFL {
  implicit class RichLogicFormula(f: QFL) {
    def toNNF(implicit converter: NNF[QFL]): NNForm =
      converter.nnf(f, b = false)
  }

  def and[A <: QFL, B <: QFL]: And[A, B, Conjunct[QFL]] = (a, b) => Conjunct(Seq(a, b))

  def or[A <: QFL, B <: QFL]: Or[A, B, Disjunct[QFL]] = (a, b) => Disjunct(Seq(a, b))

  def not[A <: QFL]: CanBuildNot[A, Not[A]] = a => Not[A](a)

  def impl[A <: QFL, B <: QFL]: Impl[A, B, Disjunct[QFL]] =
    (a, b) => Disjunct(Seq(Not[A](a), b))

  def equa[A <: QFL, B <: QFL]: Eqviv[A, B, Conjunct[QFL]] =
    (a, b) => (a impl b) && (b impl a)

  implicit def QFLConjunctAnd[A <: QFL, B <: QFL]: And[A, Conjunct[B], Conjunct[QFL]] =
    (a, b) => Conjunct(b.conj :+ a)

  implicit def ConjunctQFLAnd[A <: QFL, B <: QFL]: And[Conjunct[A], B, Conjunct[QFL]] =
    (a, b) => Conjunct(a.conj :+ b)

  implicit def ConjunctConjunctAnd[A <: QFL, B <: QFL]: And[Conjunct[A], Conjunct[B], Conjunct[QFL]] =
    (a, b) => Conjunct(a.conj ++ b.conj)

}

sealed trait Layer extends QFL

sealed trait MultiAr[A <: QFL]                    extends Layer
final case class Conjunct[A <: QFL](conj: Seq[A]) extends MultiAr[A]
object Conjunct {

  implicit def ConjunctDisjunctLiteralANDConjunctDisjunctLiteral: And[Conjunct[Disjunct[Literal[Theory]]], Conjunct[
    Disjunct[Literal[Theory]]
  ], Conjunct[Disjunct[Literal[Theory]]]] = (a, b) => Conjunct(a.conj ++ b.conj)

  implicit def ConjunctDisjunctLiteralANDDisjunctLiteral
      : And[Conjunct[Disjunct[Literal[Theory]]], Disjunct[Literal[Theory]], Conjunct[Disjunct[Literal[Theory]]]] =
    (a, b) => Conjunct(a.conj :+ b)

  implicit def DisjunctLiteralANDConjunctDisjunctLiteral
      : And[Disjunct[Literal[Theory]], Conjunct[Disjunct[Literal[Theory]]], Conjunct[Disjunct[Literal[Theory]]]] =
    (a, b) => Conjunct(b.conj :+ a)

  implicit def conjunctEQVIVliteral
      : Eqviv[Conjunct[Literal[Theory]], Literal[Theory], Conjunct[Disjunct[Literal[Theory]]]] = { (a, b) =>
    (a impl b) && (b impl a)
  }
}

final case class Disjunct[A <: QFL](disj: Seq[A]) extends MultiAr[A]
object Disjunct {
  implicit def disjunctIMPLliteral
      : Impl[Disjunct[Literal[Theory]], Literal[Theory], Conjunct[Disjunct[Literal[Theory]]]] =
    (a, b) => Conjunct(a.disj.map(l => !l || b))

  implicit def disjunctEQVIVliteral
      : Eqviv[Disjunct[Literal[Theory]], Literal[Theory], Conjunct[Disjunct[Literal[Theory]]]] = (a, b) =>
    (a impl b) && (b impl a)

  implicit def DisjunctLiteralORLiteral[A <: Theory, B <: Theory]
      : Or[Disjunct[Literal[Theory]], Literal[A], Disjunct[Literal[Theory]]] =
    (a, b) => Disjunct(a.disj :+ b)
}

final case class Not[A <: QFL](elem: A) extends QFL
object Not {
  def apply[A <: QFL, To <: QFL](a: A)(implicit cb: CanBuildNot[A, To]): To = cb.make(a)

  implicit def notNot: CanBuildNot[Not[QFL], QFL] = _.elem

}

case class Atom[+A <: Theory](theoryVar: A) extends Literal[A] {
  def lift: NnfAtom[A] = NnfAtom(theoryVar)
}
object Atom {
  implicit object showAtom extends Show[Atom[Theory]] {
    override def show(lit: Atom[Theory]): String = s"${lit.theoryVar.toString}"
  }

  implicit def notAtom: CanBuildNot[Atom[Theory], Literal[Theory]] = a => NotAtom(a.theoryVar)
}

case class NotAtom[+A <: Theory](theoryVar: A) extends Literal[A] {
  def lift: NnfNotAtom[A] = NnfNotAtom(theoryVar)
}
object NotAtom {
  implicit object showNotAtom extends Show[NotAtom[Theory]] {
    override def show(lit: NotAtom[Theory]): String = s"!${lit.theoryVar.toString}"
  }
}

sealed trait Literal[+A <: Theory] extends Layer {
  def makeAtom: Atom[A] = this match {
    case a @ Atom(atom)   => a
    case NotAtom(notAtom) => Atom(notAtom)
  }
}
object Literal {

  implicit object showLiteral extends Show[Literal[Theory]] {
    override def show(lit: Literal[Theory]): String = lit match {
      case a @ Atom(atom)        => a.show
      case na @ NotAtom(notAtom) => na.show
    }
  }

  implicit val notLiteral: CanBuildNot[Literal[Theory], Literal[Theory]] = {
    case a @ Atom(_) =>
      NotAtom(a.theoryVar)
    case NotAtom(notAtom) =>
      Atom(notAtom)
  }

  implicit def literalORliteral[A <: Theory, B <: Theory]: Or[Literal[A], Literal[B], Disjunct[Literal[Theory]]] =
    (a, b) => Disjunct(Seq(a, b))

  implicit def LiteralORDisjunctLiteral[A <: Theory, B <: Theory]
      : Or[Literal[A], Disjunct[Literal[Theory]], Disjunct[Literal[Theory]]] =
    (a, b) => Disjunct(b.disj :+ a)

  implicit def literalIMPLconjunct
      : Impl[Literal[Theory], Conjunct[Literal[Theory]], Conjunct[Disjunct[Literal[Theory]]]] = (a, b) =>
    Conjunct(b.conj.map(l => (!a || l)))

  implicit def conjunctIMPLliteral: Impl[Conjunct[Literal[Theory]], Literal[Theory], Disjunct[Literal[Theory]]] =
    (a, b) => Disjunct(a.conj.map(l => !l) :+ b)

  implicit def literalIMPLdisjunct: Impl[Literal[Theory], Disjunct[Literal[Theory]], Disjunct[Literal[Theory]]] =
    (a, b) => Disjunct(b.disj :+ (!a))

  implicit def literalEQVIVconjunct
      : Eqviv[Literal[Theory], Conjunct[Literal[Theory]], Conjunct[Disjunct[Literal[Theory]]]] = (a, b) =>
    (a impl b) && (b impl a)

  implicit def literalEQVIVdisjunct
      : Eqviv[Literal[Theory], Disjunct[Literal[Theory]], Conjunct[Disjunct[Literal[Theory]]]] = (a, b) =>
    (a impl b) && (b impl a)
}
