package net.tqft.toolkit.algebra.diophantine

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.polynomials._
import scala.collection.GenSeq
import scala.collection.GenTraversableOnce

class BoundedDiophantineSolver2[V: Ordering] extends net.tqft.toolkit.Logging {
  type P = MultivariablePolynomial[Int, V]
  import net.tqft.toolkit.algebra.polynomials.MultivariablePolynomial._
  
  val polynomialAlgebra: MultivariablePolynomialAlgebraOverGCDRing[Int, V] = implicitly

  // not exactly integer polynomial programming;
  // we try to find positive integer roots of the polynomials

  case class PolynomialProblem(equations: Seq[P], substitutions: Map[V, P]) {
	equations.foreach(e => require((e.variables intersect substitutions.keySet).isEmpty, "Equation " + e + " contains already substituted variables!\n" + this))
    
    def addSubstitution(v: V, k: Int): Option[PolynomialProblem] = {
      addSubstitution(v, polynomialAlgebra.constant(k))
    }
    def addSubstitution(v: V, p: P): Option[PolynomialProblem] = {
      require(!p.variables.contains(v), "illegal addSubstitution(" + v + ", " + p + ")")
      
      if (substitutions.keySet.contains(v)) {
        val updated = if (p.totalDegree.getOrElse(0) < substitutions(v).totalDegree.getOrElse(0)) {
          copy(substitutions = substitutions + (v -> p))
        } else {
          this
        }

        updated.addEquation(polynomialAlgebra.subtract(p, substitutions(v)))
      } else {
        val newSubstitutions = substitutions.mapValues(q => polynomialAlgebra.substitute(Map(v -> p))(q))
        val (toReprocess, toKeep) = equations.partition(_.variables.contains(v))

        PolynomialProblem(toKeep, newSubstitutions + (v -> p)).addEquations(toReprocess.map(polynomialAlgebra.substitute(Map(v -> p))))
      }
    }
    def addEquations(qs: TraversableOnce[P]): Option[PolynomialProblem] = {
      qs.foldLeft[Option[PolynomialProblem]](Some(this))({ (o, e) => o.flatMap(_.addEquation(e)) })
    }
    def addEquation(q: P): Option[PolynomialProblem] = {
      val p = polynomialAlgebra.primitivePart(polynomialAlgebra.substitute(substitutions)(q))

      def splitIfPositiveOrOtherwiseAdd: Option[PolynomialProblem] = {
        if (p.coefficients.size > 1 && (p.coefficients.forall(_._2 > 0) || p.coefficients.forall(_._2 < 0))) {
          addEquations(p.coefficients.iterator.map(t => polynomialAlgebra.monomial(t._1)))
        } else {
          add
        }
      }

      def add: Some[PolynomialProblem] = {
        if (equations.contains(p)) {
          Some(this)
        } else {
          Some(copy(equations = p +: equations))
        }
      }

      p.totalDegree match {
        case None => Some(this)
        case Some(0) => {
          val r = p.constantTerm
          require(r != 0)
          None
        }
        case Some(1) => {
          if (p.coefficients.size == 1) {
            val t = p.coefficients.head
            require(t._2 != 0)
            val v = t._1.keysIterator.next
            addSubstitution(v, 0)
          } else if (p.coefficients.size == 2) {
            if (p.constantTerm != 0) {
              // a+bV == 0
              val a = p.constantTerm
              val t = p.termsOfDegree(1).head
              val v = t._1.keysIterator.next
              val b = t._2
              if (a % b != 0 || a / b > 0) {
                None
              } else {
                addSubstitution(v, -a / b)
              }
            } else {
              // a_1 v_1 + a_2 v_2 == 0
              val Seq(t1, t2) = p.coefficients.toSeq
              val a1 = t1._2
              val a2 = t2._2
              val v1 = t1._1.keysIterator.next
              val v2 = t2._1.keysIterator.next
              if (a1 % a2 == 0) {
                if (a1 / a2 > 0) {
                  addSubstitution(v2, 0)
                } else {
                  addSubstitution(v2, polynomialAlgebra.monomial(Map(v1 -> 1), -a1 / a2))
                }
              } else {
                if (a2 % a1 == 0) {
                  if (a2 / a1 > 0) {
                    addSubstitution(v1, 0)
                  } else {
                    addSubstitution(v1, polynomialAlgebra.monomial(Map(v2 -> 1), -a2 / a1))
                  }
                } else {
                  splitIfPositiveOrOtherwiseAdd
                }
              }
            }
          } else {
            // nothing to do now
            splitIfPositiveOrOtherwiseAdd
          }
        }
        case Some(2) => {
          p.coefficients.size match {
            case 1 => {
              val h = p.coefficients.head
              // just one term
              require(h._2 != 0)
              val keys = h._1.keys.toSeq
              if (keys.size == 1) {
                addSubstitution(keys(0), 0)
              } else {
                // x y
                add
              }
            }
            case 2 => {
              val Seq(t1, t2) = p.coefficients.toSeq.sortBy(_._1.values.sum)
              require(t1._1.values.sum <= t2._1.values.sum)
              if (t1._1.values.sum == 0) {
                if (t2._1.keys.size == 1) {
                  // k + l v^2
                  val k = t1._2
                  val l = t2._2
                  if (k % l != 0) {
                    None
                  } else {
                    val v = t2._1.keys.head
                    k / l match {
                      case m if m > 0 => None
                      case 0 => addSubstitution(v, 0)
                      case -1 => addSubstitution(v, 1)
                      case -2 | -3 => None
                      case -4 => addSubstitution(v, 2)
                      case _ => add
                    }
                  }
                } else if (t2._1.keys.size == 2) {
                  // k + l uv
                  val k = t1._2
                  val l = t2._2
                  if (k % l != 0) {
                    None
                  } else {
                    val List(u, v) = t2._1.keys.toList
                    k match {
                      case k if k > 0 => None
                      case -1 => addSubstitution(u, 1).flatMap(_.addSubstitution(v, 1))
                      case _ => add
                    }
                  }
                } else {
                  splitIfPositiveOrOtherwiseAdd
                }
              } else if (t1._1.values.sum == 2 && t1._1.keys.size == 1 && t2._1.values.sum == 2 && t2._1.keys.size == 1) {
                // a x^2 + b x^y
                val a = t1._2
                val b = t2._2
                val x = t1._1.keys.head
                val y = t2._1.keys.head
                if (a == -b) {
                  addSubstitution(x, polynomialAlgebra.monomial(y))
                } else if (a == b) {
                  addSubstitution(x, 0).flatMap(_.addSubstitution(y, 0))
                } else {
                  splitIfPositiveOrOtherwiseAdd
                }
              } else {
                splitIfPositiveOrOtherwiseAdd
              }
            }
            case 3 => {
              val Seq(t1, t2, t3) = p.coefficients.toSeq.sortBy(t => (t._1.values.sum, t._2))
              if (t1._1.values.sum == 0 && t2._1.values.sum == 2 && t3._1.values.sum == 2 && t2._1.keys.size == 1 && t3._1.keys.size == 1) {
                val x = t2._1.keys.head
                val y = t3._1.keys.head
                if (t1._2 == -2 && t2._2 == 1 & t3._2 == 1) {
                  // -2 + x^2 + y^2
                  addSubstitution(x, 1).flatMap(_.addSubstitution(y, 1))
                } else if (t1._2 == -1 && t2._2 == 1 && t3._2 == 2) {
                  // -1 + x^2 + 2 y^2
                  addSubstitution(x, 1).flatMap(_.addSubstitution(y, 0))
                } else {
                  splitIfPositiveOrOtherwiseAdd
                }
              } else {
                splitIfPositiveOrOtherwiseAdd
              }
            }
            case _ => {
              splitIfPositiveOrOtherwiseAdd
            }
          }
        }
        case Some(d) => splitIfPositiveOrOtherwiseAdd
      }
    }

    private def solveALinearEquation: Option[PolynomialProblem] = {
      def solve(p: P): Option[PolynomialProblem] = {
        val t = p.termsOfDegree(1).find(_._2 == 1).get
        val v = t._1.keysIterator.next
        addSubstitution(v, polynomialAlgebra.subtract(polynomialAlgebra.monomial(v), p))
      }

      equations.find(p => p.totalDegree == Some(1) && p.termsOfDegree(1).exists(_._2 == 1) && p.coefficients.count(_._2 > 0) == 1) match {
        case Some(p) => solve(p)
        case None => {
          equations.find(p => p.totalDegree == Some(1) && p.termsOfDegree(1).exists(_._2 == -1) && p.coefficients.count(_._2 < 0) == 1) match {
            case Some(p) => solve(polynomialAlgebra.negate(p))
            case None => Some(this)
          }
        }
      }
    }
    def solveLinearEquations: Option[PolynomialProblem] = {
      import net.tqft.toolkit.functions.FixedPoint
      FixedPoint({ o: Option[PolynomialProblem] => o.flatMap(_.solveALinearEquation) })(Some(this))
    }

    def caseBashCompletely(variables: Seq[V], boundary: (V =>? Int) => Boolean): Iterator[Map[V, Int]] = {
      caseBashVariables(variables, boundary).map(p => p.substitutions.mapValues(_.ensuring(_.totalDegree.getOrElse(0) == 0).constantTerm))
    }
    def caseBashEquations(boundary: (V =>? Int) => Boolean): Iterator[PolynomialProblem] = {
      if (equations.isEmpty) {
        Iterator(this)
      } else {
        caseBashOnce(equations.head.variables.head, boundary).flatMap(_.caseBashEquations(boundary))
      }
    }
    def caseBashVariables(variables: Seq[V], boundary: (V =>? Int) => Boolean): Iterator[PolynomialProblem] = {
      variables.filterNot(substitutions.keySet) match {
        case v +: remainingVariables => {
          caseBashOnce(v, boundary).flatMap(_.caseBashVariables(remainingVariables, boundary))
        }
        case _ => Iterator(this)
      }
    }
    private def caseBashOnce(v: V, boundary: (V =>? Int) => Boolean): Iterator[PolynomialProblem] = {
      // FIXME this has failed in the wild, having arrived from caseBashEquations.
      require(!substitutions.keySet(v))

      def minimalSubstitution(k: Int) = {
        substitutions.mapValues(p => polynomialAlgebra.completelySubstituteConstants(Map(v -> k))(p)) ++ Map(v -> k)
      }

      Iterator.from(0).takeWhile({ k => boundary(minimalSubstitution(k)) }).flatMap(i => addSubstitution(v, i).flatMap(_.solveLinearEquations))
    }
  }

  //  trait Boundary extends (Map[V, Int] => Boolean) {
  //    def addSubstitution(v: V, p: P): Boundary
  //  }
  //  object Boundary {
  //    implicit def lift(f: Map[V, Int] => Boolean): Boundary = {
  //      case class CachingBoundary(constantSubstitutions: Map[V, Int], polynomialSubstitutions: Map[V, P]) extends Boundary {
  //        def apply(m: Map[V, Int]) = {
  //          f(m ++ constantSubstitutions ++ polynomialSubstitutions.mapValues(polynomialAlgebra.completelySubstituteConstants(m)(_)))
  //        }
  //        override def addSubstitution(v: V, p: P): Boundary = {
  //          val (newConstants, newPolynomials) = (polynomialSubstitutions.mapValues(polynomialAlgebra.substitute(Map(v -> p))(_)) + ( v-> p)).partition(_._2.totalDegree.getOrElse(0) == 0)
  //          CachingBoundary(constantSubstitutions ++ newConstants.mapValues(_.constantTerm), newPolynomials)
  //        }
  //      }
  //      CachingBoundary(Map(), Map())
  //    }
  //  }

  def solve(polynomials: TraversableOnce[MultivariablePolynomial[Int, V]]): Option[PolynomialProblem] = {
    PolynomialProblem(Seq.empty, Map.empty).addEquations(polynomials).flatMap(_.solveLinearEquations)
  }

}