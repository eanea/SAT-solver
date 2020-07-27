sealed trait Theory extends Any with Product with Serializable
object Theory {
  def atom(name: String): Literal[Theory]    = Atom(Var(name))
  def notAtom(name: String): Literal[Theory] = NotAtom(Var(name))
  def rval: Rvar = Rvar.apply
}
final case class Rvar(id: Int) extends AnyVal with Theory {
  override def toString: String = s"r$id"
}
private object Rvar {
  private var id = 0

  def apply: Rvar = {
    id = id + 1
    new Rvar(id)
  }
}

final case class Var(name: String) extends AnyVal with Theory {
  override def toString: String = name
}