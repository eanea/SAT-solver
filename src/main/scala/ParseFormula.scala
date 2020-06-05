object ParseFormula extends App {
  def getLinear(f: LinearFunction): Map[Int, Double] = {

    def go(ff: LinearFunction,
           ind: Vector[Int],
           vals: Vector[Double]): (Seq[Int], Seq[Double]) = {
      ff match {
        case Variable(name, id, k) => (ind :+ id, vals :+ k)
        case Diff(f1, f2) => {
          val a1 = go(f1, ind, vals)
          val a2 = go(f2, ind, vals)
          (a1._1 concat a2._1, a1._2 concat a2._2.map(_ * -1))
        }
        case Neg(f) => {
          val r = go(f, ind, vals)
          (r._1, r._2.map(_ * -1))
        }
        case s@Sum(seq) => {
          seq
            .map { x =>
              {
                go(x, ind, vals)
              }
            }
            .foldLeft((Seq[Int]() -> Seq[Double]()))(
              (acc, x) => (acc._1 concat x._1, acc._2 concat x._2)
            )
        }
        case Pair(q, k) => {
          (Seq(), Seq())
        }
      }
    }
    val (inds, vals) = go(f, Vector.empty, Vector.empty)
    inds.zip(vals).sortBy(_._2).toMap
  }

  private def getLogic(f: Literal[Less],
                       isStrict: Boolean = true): (Map[Int, Double], Pair) =
    f match {
      case Atom(less) => less match {
        case Less(l: LinearFunction, p: Pair) =>
          getLinear(l) -> p
        case Less(p : Pair, l: LinearFunction) =>
          getLinear(l).map { case (i, d) => i -> (d * -1) } -> p * -1
      }
      case NotAtom(less) =>
        val l = getLogic(Atom(less), isStrict = false)
        l._1 -> l._2

    }

  private def parseCons(cons: Vector[Literal[Less]]):
  (Seq[Map[Int, Double]], Seq[Pair]) = {
    val l = cons.map(getLogic(_))
    val coeffs = l.map(_._1)
    val b = l.map(_._2)
    coeffs -> b
  }


  def getTableay(cons: Vector[Literal[Less]]): (Map[Int, Seq[Double]], Map[Int, Pair], Int, Int) = {
    val (parsedTable, b) = parseCons(cons)
    val c = cons.length
    val (aMap, dim) = mkTableau(parsedTable, b, c)
    val xMap = Range(dim + 1, dim + c + 1).zip(b).toMap
    (aMap, xMap, dim, c)
  }

  def mkTableau(parsedFormula: Seq[Map[Int, Double]],
                   b: Seq[Pair], consDim: Int): (Map[Int, Seq[Double]], Int) = {
    val numberOfVar = {
      for {
        row <- parsedFormula
      } yield row.maxBy(_._1)._1
    }.max

    @scala.annotation.tailrec
    def go(i: Int, map: Map[Int, Seq[Double]]): Map[Int, Seq[Double]] = {
      if (i > numberOfVar) map
      else {
        val column = for {
          row <- parsedFormula
        } yield row.getOrElse(i, 0d)
        go(i + 1, map.updated(i, column))
      }
    }

    go(1, Map.empty) -> numberOfVar

  }

  def parse(z: LinearFunction, cons: Vector[Literal[Less]]): (Map[Int, Pair], Map[Int, Double], Map[Int, Seq[Double]], Int, Int) = {
    val (aMap, xMap, dim, constDim) = getTableay(cons)
    val cMap = getLinear(z)
    (xMap, cMap, aMap, dim, constDim)
  }
}
