package net.tqft.toolkit.algebra
import net.tqft.toolkit.collections.KSubsets
import net.tqft.toolkit.collections.NonStrictIterable
import net.tqft.toolkit.Logging

trait LinearProgrammingHelper extends Logging {
  //  def initialSimplex[B](m: Matrix[B], c: List[B])(implicit field: Field[B]): List[Int] = {
  //    KSubsets(m.numberOfColumns, m.numberOfRows) filter { simplex => m.takeColumns(simplex).preimageOf(c).nonEmpty } head
  //  }

  def initialSimplex[B](m: Matrix[B])(implicit field: Field[B]): List[Int] = {
    m.findBasisForColumnSpace()
  }

  def adjacentSimplices(simplex: List[Int], exitingColumn: Int, numberOfColumns: Int): Iterable[List[Int]] = {
    import net.tqft.toolkit.collections.Rotate._

    for (
      enteringColumn <- ((0 until numberOfColumns).toList filterNot (simplex contains _)) rotateLeft (simplex.hashCode)
    ) yield {
      (enteringColumn :: (simplex filterNot (_ == exitingColumn))).sorted
    }
  }
  def adjacentSimplices(simplex: List[Int], numberOfColumns: Int): Iterable[List[Int]] = {
    import net.tqft.toolkit.collections.Rotate._

    for (
      exitingColumn <- simplex;
      as <- adjacentSimplices(simplex, exitingColumn, numberOfColumns)
    ) yield as
  }

}

object NotTheSimplexAlgorithm extends LinearProgrammingHelper {
  def slack[B](solution: Seq[B])(implicit field: OrderedField[B]) = {
    import AlgebraicNotation._
    field.negate(sum(solution filter { x => field.compare(x, field.zero) < 0 })(field))
  }

  def apply[B](m: Matrix[B], c: List[B])(implicit field: OrderedField[B]) = {

    val simplex0 = m.findBasisForColumnSpace()

    info("Generated initial simplex: " + simplex0)

    def solution(simplex: List[Int]) = m.takeColumns(simplex).preimageOf(c).get

    val (simplex1, solution1) = NonStrictIterable.iterateUntilNone((simplex0, solution(simplex0))) { case (si, sl) => replaceNegativeColumn(si, sl, m) map { nsi => (nsi, solution(nsi)) } } last

    (simplex1, slack(solution1), solution1)
  }

  def replaceNegativeColumn[B](simplex: List[Int], solution: Seq[B], m: Matrix[B])(implicit field: OrderedField[B]): Option[List[Int]] = {
    info("replacing: " + simplex + " with slack " + slack(solution))

    val possibilities = for (
      (x, k) <- solution zipWithIndex;
      if field.compare(x, field.zero) < 0;
      exitingColumn = simplex(k)
    ) yield {
      (for (
        enteringColumn <- (0 until m.numberOfColumns).toList filterNot (simplex contains _);
        preimage <- m.takeColumns(simplex).preimageOf(m.takeColumn(enteringColumn).toList);
        if field.compare(preimage(k), field.zero) < 0
      ) yield (enteringColumn :: (simplex filterNot (_ == exitingColumn))).sorted).headOption
    }

    if (possibilities.isEmpty || possibilities.contains(None)) {
      None
    } else {
      possibilities.head
    }

  }
}

object SimplexAlgorithm extends LinearProgrammingHelper {

  def slack[B](simplex: List[Int], m: Matrix[B], c: List[B])(implicit field: OrderedField[B]): Option[B] = {
    import AlgebraicNotation._

    m.takeColumns(simplex).preimageOf(c) match {
      case None => None
      case Some(v) => {
        val result = field.negate(sum(v filter { x => field.compare(x, field.zero) < 0 }))
        //        info("Calculated slack " + result + " for  " + simplex)
        Some(result)
      }
    }
  }

  def decreaseSlack[B](oldSimplex: List[Int], oldSlack: B, m: Matrix[B], c: List[B])(implicit field: OrderedField[B]): Option[(List[Int], B)] = {
    info("Trying to decrease the slack, from: " + oldSimplex)

    if (oldSlack == field.zero) {
      None
    } else {
      val adjacentSimplicesWithSlack = adjacentSimplices(oldSimplex, m.numberOfColumns) map { newSimplex => (newSimplex, slack(newSimplex, m, c)) }

      val betterSimplices = adjacentSimplicesWithSlack collect { case (newSimplex, Some(newSlack)) if field.compare(newSlack, oldSlack) < 0 => (newSimplex, newSlack) }

      betterSimplices.headOption
    }
  }

  /* either finds an adjacent simplex with lower slack, or an iterable of adjacent simplices with the same slack */
  def tryToDecreaseSlack[B](oldSimplex: List[Int], oldSlack: B, m: Matrix[B], c: List[B])(implicit field: OrderedField[B]): Either[(List[Int], B), Iterable[List[Int]]] = {
    info("Trying to decrease the slack, from: " + oldSimplex + ", with slack: " + oldSlack)

    if (oldSlack == field.zero) {
      Right(None)
    } else {
      val adjacentSimplicesWithSlack = adjacentSimplices(oldSimplex, m.numberOfColumns) map { newSimplex => (newSimplex, slack(newSimplex, m, c)) }

      val noWorseSimplices = adjacentSimplicesWithSlack collect { case (newSimplex, Some(newSlack)) if field.compare(newSlack, oldSlack) <= 0 => (newSimplex, newSlack) }
      val (betterSimplices, sameSimplices) = noWorseSimplices partition { case (_, newSlack) => field.compare(newSlack, oldSlack) < 0 }

      betterSimplices.headOption match {
        case Some((simplex, slack)) => Left((simplex, slack))
        case None => Right(sameSimplices map { _._1 })
      }
    }
  }

  def tryHarderToDecreaseSlack[B](oldSimplex: List[Int], oldSlack: B, m: Matrix[B], c: List[B])(implicit field: OrderedField[B]): Option[(List[Int], B)] = {
    val simplicesConsidered = scala.collection.mutable.HashSet[List[Int]]()
    val simplicesToDo = scala.collection.mutable.Queue[List[Int]](oldSimplex)

    var result: Option[(List[Int], B)] = None

    while (result.isEmpty && simplicesToDo.nonEmpty) {
      info(simplicesConsidered.size + " " + simplicesToDo.size)

      val next = simplicesToDo.dequeue
      simplicesConsidered += next
      tryToDecreaseSlack(next, oldSlack, m, c) match {
        case Left(p) => {
          result = Some(p)
        }
        case Right(i) => {
          for (s <- i; if !simplicesConsidered.contains(s) && !simplicesToDo.contains(s)) {
            simplicesToDo.enqueue(s)
          }
        }
      }
    }

    result
  }

  def apply[B](m: Matrix[B], c: List[B])(implicit field: OrderedField[B]) = {
    val simplex0 = initialSimplex(m)
    info("Generated initial simplex: " + simplex0)

    val slack0 = slack(simplex0, m, c).get
    info("   with initial slack: " + slack0)

    val (simplex1, slack1) = NonStrictIterable.iterateUntilNone((simplex0, slack0)) { case (si, sl) => tryHarderToDecreaseSlack[B](si, sl, m, c) } last

    (simplex1, slack1, m.takeColumns(simplex1).preimageOf(c).get)
  }
}

object CommonsSimplexSolver {
  import org.apache.commons.math.optimization.linear._
  import org.apache.commons.math.optimization.GoalType
  import scala.collection.JavaConversions._

  def apply(m: Matrix[Double], c: List[Double]) = {
    val constraints = (for ((row, x) <- m.entries zip c) yield {
      new LinearConstraint(row.toArray, Relationship.EQ, x)
    }).toList
    val function = new LinearObjectiveFunction(List.fill(m.numberOfColumns)(0.0).toArray, 0.0)

    val result =
      try {
        val solver = new SimplexSolver()
        solver.setMaxIterations(Integer.MAX_VALUE)
        Some(solver.optimize(function, constraints, GoalType.MAXIMIZE, true))
      } catch {
        case e: org.apache.commons.math.optimization.linear.NoFeasibleSolutionException => None
      }

    result map { r => r.getPoint.toList }
  }

}