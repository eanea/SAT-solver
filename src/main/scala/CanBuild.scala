object CanBuild {
  trait And[A, B, To] {
    def make(a: A, b: B): To
  }

  trait Or[A, B, To] {
    def make(a: A, b: B): To
  }

  trait Impl[A, B, To] {
    def make(a: A, b: B): To
  }

  trait Eqviv[A, B, To] {
    def make(a: A, b: B): To
  }

  trait CanBuildNot[A, To] {
    def make(a: A): To
  }
}
