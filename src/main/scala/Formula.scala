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

/*
Theory
 */
trait Theory extends Any with Product with Serializable
object Theory {
  def atom(name: String): Literal[Theory]    = Atom(Var(name))
  def notAtom(name: String): Literal[Theory] = NotAtom(Var(name))

  // как отличать Rval от Var
  def rval = Rvar.apply
}
final case class Rvar(id: Int) extends AnyVal with Theory {
  override def toString: String = s"r$id"
}
private object Rvar {
  private var id = 0

  def apply = {
    id = id + 1
    new Rvar(id)
  }
}

final case class Var(name: String) extends AnyVal with Theory {
  override def toString: String = name
}

sealed trait Linear                                           extends Any with Theory
sealed trait LinearIneq                                       extends Any with Linear
final case class Less(f1: LinearFunction, f2: LinearFunction) extends LinearIneq
sealed trait LinearFunction extends Any with Linear {
  def +(that: LinearFunction): Sum  = Sum(this, that)
  def -(that: LinearFunction): Diff = Diff(this, that)
  def unary_- : LinearFunction      = Neg(this)
  def <(k: Double): Literal[Less]   = Atom(Less(this, Pair(k, -1)))
  def >(k: Double): Literal[Less]   = Atom(Less(Pair(k, 1), this))
  def <=(k: Double): Literal[Less]  = NotAtom(Less(this, Pair(k)))
  def >=(k: Double): Literal[Less]  = NotAtom(Less(Pair(k), this))
  /*
  or Conjunct[Litereal[Theory]] ???
   */
  def =|(k: Double): Conjunct[QFL] = this >= k && this <= k
}

final case class Sum(sum: Seq[LinearFunction]) extends AnyVal with LinearFunction
object Sum {
  def apply(f1: LinearFunction, f2: LinearFunction): Sum = (f1, f2) match {
    case (x: Sum, y: Sum) => Sum(x.sum ++ y.sum)
    case (x, y: Sum)      => Sum(y.sum :+ x)
    case (x: Sum, y)      => Sum(x.sum :+ y)
    case _                => Sum(Seq(f1, f2))
  }
}

final case class Diff(f1: LinearFunction, f2: LinearFunction) extends LinearFunction
final case class Neg(f: LinearFunction)                       extends LinearFunction
object Neg {
  def apply(f: LinearFunction): LinearFunction = f match {
    case Neg(f)                => f
    case Variable(name, id, k) => Variable(name, id, -k)
    case _                     => new Neg(f)
  }
}

object Variable {
  private var id = 0

  def apply(name: String): Variable = {
    id = id + 1
    Variable(name, id)
  }

  implicit object showVariable extends Show[Variable] {
//    override def show(v: Variable): String = s"${v.k} * ${v.name}${v.id}"
//    override def show(v: Variable): String = s"${v.k} * ${v.name}"
    override def show(v: Variable): String = s"${v.name}"
  }
}
final case class Variable(name: String, id: Int, k: Double = 1.0) extends LinearFunction {
  def *(coeff: Double) = Variable(name, id, k * coeff)

  override def toString = this.show
}

final case class Pair(q: Double, k: Double) extends Ordered[Pair] with LinearFunction {
  def +(that: Pair): Pair = Pair(q + that.q, k + that.k)
  def -(that: Pair): Pair = Pair(q - that.q, k - that.k)

  def *(a: Double): Pair = Pair(q * a, k * a)
  def /(a: Double): Pair = Pair(q / a, k / a)

  override def compare(that: Pair): Int = {
    if (q == that.q) {
      if (Math.abs(k - that.k) < 0.0000001) 0
      else k compare that.k
    } else {
      if (Math.abs(q - that.q) < 0.0000001) 0
      else q compare that.q
    }
  }
}
object Pair {
  def apply(q: Double): Pair = new Pair(q, 0)
  def zero: Pair             = Pair(0d, 0d)
  implicit object showPair extends Show[Pair] { //s"$q" else s"$q + $k"
    override def show(pair: Pair): String = s"${pair.q} + ${pair.k}"
  }
}

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

final case class CNForm(cnf: Conjunct[Disjunct[Literal[Theory]]])
