package net.tqft.toolkit.algebra

object IntegerPolynomialProgramming {
  // not exactly integer polynomial programming;
  // we try to find positive integer roots of the polynomials
  def solve[V: Ordering](polynomials: Seq[MultivariablePolynomial[Int, V]]): (Iterable[Map[V, Int]], Iterable[Seq[MultivariablePolynomial[Int, V]]]) = {

    // this "BogusScope" protects us from https://issues.scala-lang.org/browse/SI-6231
    object BogusScope {
      type P = MultivariablePolynomial[Int, V]

      case class PartialSolution(substitutions: Map[V, P], remainingPolynomials: Seq[P]) {
//        require((remainingPolynomials.flatMap(_.variables).toSet.intersect(substitutions.keySet)).isEmpty)

        def add(newSubstitutions: Map[V, P]): PartialSolution = {
//          require(newSubstitutions.values.toSet[P].flatMap(_.variables).intersect(newSubstitutions.keySet).isEmpty)

          def substitute(p: P): P = {
            polynomialAlgebra.substitute(newSubstitutions)(p)
          }
          def substituteSeq(polynomials: Seq[P]): Seq[P] = {
            (for (
              p <- polynomials.par;
              q = substitute(p);
              if q.nonZero
            ) yield { q }).seq
          }

          val updatedSubstitutions = substitutions.mapValues(p => substitute(p))
//          val differences = {
//            for (
//              v <- updatedSubstitutions.keySet.intersect(newSubstitutions.keySet);
//              d = polynomialAlgebra.subtract(updatedSubstitutions(v), newSubstitutions(v));
//              if d.nonZero
//            ) yield d
//          }
//
//          require(differences.isEmpty)

          val substitutedPolynomials = substituteSeq(remainingPolynomials)
          PartialSolution(newSubstitutions ++ updatedSubstitutions, substitutedPolynomials/* ++ differences*/)
        }
      }

      val polynomialAlgebra = MultivariablePolynomialAlgebras.over[Int, V]
      implicit def constant(k: Int) = polynomialAlgebra.constant(k)

      trait Strategy {
        def apply(sol: PartialSolution): Option[Iterable[PartialSolution]]
      }

      trait SolveOneAtATimeStrategy extends Strategy {
        protected val findSubstitutions: PartialFunction[P, Iterable[Map[V, P]]]
        private lazy val memo = {
          import net.tqft.toolkit.functions.Memo._
          findSubstitutions.lift.memo
        }
        private def findSomeSubstitutions(ps: Seq[P]): Option[Iterable[Map[V, P]]] = {
          ps.iterator.map(memo).find(_.nonEmpty).getOrElse(None)
        }
        override def apply(sol: PartialSolution) = findSomeSubstitutions(sol.remainingPolynomials) map { iterable =>
          for (newSubstitutions <- iterable) yield {
            sol.add(newSubstitutions).ensuring(_ != sol)
          }
        }
      }
      trait ReplacingStrategy extends Strategy {
        protected val findReplacements: PartialFunction[P, Seq[P]]
        private lazy val memo = {
          import net.tqft.toolkit.functions.Memo._
          findReplacements.lift.memo
        }
        override def apply(sol: PartialSolution) = {
          val (replacements, keep) = sol.remainingPolynomials.map(p => (p, memo(p))).partition(_._2.isDefined)
          if (replacements.isEmpty) {
            None
          } else {
            val next = PartialSolution(sol.substitutions,
              replacements.flatMap(_._2.get) ++ keep.map(_._1))
            require(sol != next)
            Some(Iterable(next))
          }
        }
      }

      case object SplitPositiveLinearCombinations extends ReplacingStrategy {
        override val findReplacements: PartialFunction[P, Seq[P]] = {
          case p if p.terms.size > 1 && p.terms.forall(_._2 > 0) => p.terms.map({ case (m, a) => polynomialAlgebra.monomial(m) })
        }
      }
      case object `a=0` extends SolveOneAtATimeStrategy {
        override val findSubstitutions: PartialFunction[P, Iterable[Map[V, P]]] = {
          case p if p.totalDegree == Some(0) => {
            require(p.constantTerm != 0)
            Iterable.empty
          }
        }
      }
      case object `V^k=0` extends SolveOneAtATimeStrategy {
        override val findSubstitutions: PartialFunction[P, Iterable[Map[V, P]]] = {
          case MultivariablePolynomial((m, a)) if m.size == 1 => {
            require(a != 0)
            Iterable(Map(m.keysIterator.next -> 0))
          }
        }
      }
      case object `a+bV=0` extends SolveOneAtATimeStrategy {
        override val findSubstitutions: PartialFunction[P, Iterable[Map[V, P]]] = {
          case MultivariablePolynomial((m0, a), (m1, b)) if m0.isEmpty && m1.values.sum == 1 => {
            val v = m1.keysIterator.next
            if (a % b != 0) {
              Iterable.empty
            } else {
              val r = -a / b
              if (r < 0) {
                Iterable.empty
              } else {
                Iterable(Map(v -> r))
              }
            }
          }
        }
      }
      case object `V+aW=0` extends SolveOneAtATimeStrategy {
        override val findSubstitutions: PartialFunction[P, Iterable[Map[V, P]]] = {
          case p @ MultivariablePolynomial((m0, a), (m1, b)) if p.totalDegree == Some(1) && (a.abs == 1 || b.abs == 1) => {
            if (a.abs == 1) {
              val v = m0.keysIterator.next
              Iterable(Map(v -> polynomialAlgebra.monomial(m1, -b / a)))
            } else {
              val v = m1.keysIterator.next
              Iterable(Map(v -> polynomialAlgebra.monomial(m0, -a / b)))
            }
          }
        }
      }
      case object LinearWithUnitCoefficient extends SolveOneAtATimeStrategy {
        override val findSubstitutions: PartialFunction[P, Iterable[Map[V, P]]] = {
          case p if p.totalDegree == Some(1) && p.termsOfDegree(1).exists(_._2.abs == 1) => {
            val (m, a) = p.termsOfDegree(1).find(_._2.abs == 1).get
            val v = m.keysIterator.next
            Iterable(Map(v -> polynomialAlgebra.add(polynomialAlgebra.scalarMultiply(-1 / a, p), polynomialAlgebra.monomial(v))))
          }
        }
      }

      case class CombinedStrategy(strategies: List[Strategy]) extends Strategy {
        def apply(sol: PartialSolution) = {
          strategies match {
            case Nil => None
            case head :: tail => {
              head(sol) match {
                case None => {
                  copy(tail)(sol)
                }
                case Some(iterable) => Some(iterable)
              }
            }
          }
        }
      }
      case class RepeatingStrategy(strategy: Strategy) extends Strategy {
        def apply(sol: PartialSolution) = {
          strategy(sol).map({ iterable =>
            val newSolutions = scala.collection.mutable.ListBuffer[PartialSolution]()
            val fixedSolutions = scala.collection.mutable.ListBuffer[PartialSolution]()
            newSolutions ++= iterable
            while (newSolutions.nonEmpty) {
              val processing = newSolutions.toList
              newSolutions.clear
              for (s <- processing) {
                strategy(s) match {
                  case None => fixedSolutions += s
                  case Some(iterable) => newSolutions ++= iterable
                }
              }
            }
            fixedSolutions.toIterable
          })
        }
      }

      val strategies: List[Strategy] = List(SplitPositiveLinearCombinations, `a=0`, `V^k=0`, `a+bV=0`, `V+aW=0`, LinearWithUnitCoefficient)
      val strategy = RepeatingStrategy(CombinedStrategy(strategies))

      def result = strategy(PartialSolution(Map.empty, polynomials)) match {
        case None => {
          (Iterable.empty, Iterable(polynomials))
        }
        case Some(iterable) => {
          val (finished, unfinished) = iterable.partition(_.remainingPolynomials.isEmpty)
          val solutions = for (f <- finished) yield {
            f.substitutions.mapValues(p => p.ensuring(_.totalDegree.getOrElse(0) == 0).constantTerm)
          }
          val tooHard = unfinished.map(_.remainingPolynomials)
          (solutions, tooHard)
        }
      }
    }
    BogusScope.result
  }

}