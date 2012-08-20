package net.tqft.toolkit.algebra

object IntegerPolynomialProgramming {

  // not exactly integer polynomial programming;
  // we try to find positive integer roots of the polynomials

  // the order of variables matters; if we need to split into cases, we prefer splitting the first variables first.
  def solve[V: Ordering](polynomials: Seq[MultivariablePolynomial[Int, V]], variables: Seq[V], boundary: Option[Map[V, Int] => Boolean] = None, knownSolution: Option[Map[V, Int]] = None): (Iterable[Map[V, Int]], Iterable[Seq[MultivariablePolynomial[Int, V]]]) = {

    // this "BogusScope" protects us from https://issues.scala-lang.org/browse/SI-6231
    object BogusScope {
      type P = MultivariablePolynomial[Int, V]

      val polynomialAlgebra = MultivariablePolynomialAlgebras.over[Int, V]
      implicit def constant(k: Int) = polynomialAlgebra.constant(k)

      case class PartialSolution(substitutions: Map[V, P], remainingPolynomials: Seq[P]) {
        knownSolution.map({ solution =>
          substitutions.filter(_._2.totalDegree.getOrElse(0) == 0).map({
            case (v, p) => require(solution(v) == p.constantTerm)
          })
          val polynomials = for (p <- remainingPolynomials; q = polynomialAlgebra.substituteConstants(solution)(p); if q.nonZero) yield { q }
          require(polynomials.isEmpty)
        })

        println("new PartialSolution() with " + substitutions.size + " substitutions and " + remainingPolynomials.size + " polynomials.")
        //        require((remainingPolynomials.flatMap(_.variables).toSet.intersect(substitutions.keySet)).isEmpty)

        def remainingVariables = remainingPolynomials.flatMap(_.variables)
        def minimalSubstitution: Map[V, Int] = {
          add(remainingVariables.map(x => x -> constant(0)).toMap).substitutions.mapValues(_.constantTerm)
        }

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
          val substitutedPolynomials = substituteSeq(remainingPolynomials)
          PartialSolution(newSubstitutions ++ updatedSubstitutions, substitutedPolynomials)
        }
      }

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
          val result = ps.iterator.map(memo).find(_.nonEmpty).getOrElse(None)
          //          println(result)
          result
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
            println("found impossible polynomial: " + p)
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
          case p @ MultivariablePolynomial((m0, a), (m1, b)) if m0.isEmpty && m1.values.sum == 1 => {
            val v = m1.keysIterator.next
            if (a % b != 0) {
              println("found impossible polynomial: " + p)
              Iterable.empty
            } else {
              val r = -a / b
              if (r < 0) {
                println("found impossible polynomial: " + p)
                Iterable.empty
              } else {
                Iterable(Map(v -> r))
              }
            }
          }
        }
      }
      //      case object `V+aW=0` extends SolveOneAtATimeStrategy {
      //        override val findSubstitutions: PartialFunction[P, Iterable[Map[V, P]]] = {
      //          case p @ MultivariablePolynomial((m0, a), (m1, b)) if p.totalDegree == Some(1) && (a.abs == 1 || b.abs == 1) => {
      //            if (a.abs == 1) {
      //              val v = m0.keysIterator.next
      //              Iterable(Map(v -> polynomialAlgebra.monomial(m1, -b / a)))
      //            } else {
      //              val v = m1.keysIterator.next
      //              Iterable(Map(v -> polynomialAlgebra.monomial(m0, -a / b)))
      //            }
      //          }
      //        }
      //      }
      case object LinearWithUnitCoefficient extends SolveOneAtATimeStrategy {
        // is there a term with coefficient \pm 1, such that all other terms have the opposite sign?
        def invertibleCoefficientWithOppositeSign_?(p: P) = {
          p.termsOfDegree(1).exists(_._2 == 1) && p.terms.count(_._2 > 0) == 1 ||
            p.termsOfDegree(1).exists(_._2 == -1) && p.terms.count(_._2 < 0) == 1
        }

        override val findSubstitutions: PartialFunction[P, Iterable[Map[V, P]]] = {
          case p if p.totalDegree == Some(1) && invertibleCoefficientWithOppositeSign_?(p) => {
            val (m, a) = p.termsOfDegree(1).find(_._2.abs == 1).get
            val v = m.keysIterator.next
            Iterable(Map(v -> polynomialAlgebra.add(polynomialAlgebra.scalarMultiply(-1 / a, p), polynomialAlgebra.monomial(v))))
          }
        }
      }
      case object SplitIntoCases extends Strategy {
        def apply(ps: PartialSolution) = {
          boundary.map ({ limit =>
          val remainingVariables = ps.remainingVariables
          val v = variables.filter(remainingVariables.contains).head
          println("Splitting into cases trying values of " + v)

          val cases = Iterator.from(0).map({ k =>
            println("  considering " + v + " = " + k)
            ps.add(Map(v -> constant(k)))
          })
          cases.takeWhile(psk => limit(psk.minimalSubstitution)).toStream
          })
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
          println("beginning " + this)
          strategy(sol).map({ iterable =>
            val newSolutions = scala.collection.mutable.ListBuffer[PartialSolution]()
            val fixedSolutions = scala.collection.mutable.ListBuffer[PartialSolution]()
            newSolutions ++= iterable
            while (newSolutions.nonEmpty) {
              val processing = newSolutions.toList
              newSolutions.clear
              for (s <- processing) {
                println("trying PartialSolution() with " + s.substitutions.size + " substitutions and " + s.remainingPolynomials.size + " polynomials.")
                strategy(s) match {
                  case None => {
                    println("no further progress on " + s)
                    fixedSolutions += s
                  }
                  case Some(iterable) => {
                    val n = iterable.toList
                    println("made some further progress: " + n.size)
                    newSolutions ++= n
                  }
                }
              }
            }
            fixedSolutions.toIterable
          })
        }
      }

      val strategies: List[Strategy] = List(SplitPositiveLinearCombinations, `a=0`, `V^k=0`, `a+bV=0`, LinearWithUnitCoefficient, SplitIntoCases)
      val strategy = RepeatingStrategy(CombinedStrategy(strategies))

      def result = strategy(PartialSolution(Map.empty, polynomials)).map(_.toSeq) match {
        case None => {
          (Iterable.empty, Iterable(polynomials))
        }
        case Some(iterable) => {
          println(iterable)
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