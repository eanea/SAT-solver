object Implicits {
  implicit class RichDouble(val d: Double) extends AnyVal {
    def *(v : Variable): Variable = v.copy(k = d * v.k)
  }
}
