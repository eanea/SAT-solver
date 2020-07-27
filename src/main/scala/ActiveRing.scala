final case class ActiveRing(ring: Vector[Atom[Theory]]) extends AnyVal {
  def nextEither(trace: Trace): Either[String, Atom[Theory]] = {
    if (ring.isEmpty) Left("ActiveRing nextEither isEmpty")
    else
      Right(ring.head)
  }
}