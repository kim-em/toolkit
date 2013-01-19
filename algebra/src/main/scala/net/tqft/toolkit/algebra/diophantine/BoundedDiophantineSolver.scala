package net.tqft.toolkit.algebra.diophantine

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.polynomials._
import scala.collection.GenSeq
import scala.collection.GenTraversableOnce

object BoundedDiophantineSolver extends net.tqft.toolkit.Logging {

  // not exactly integer polynomial programming;
  // we try to find positive integer roots of the polynomials

  // TODO make boundary compulsory
  def solve[V: Ordering](polynomials: TraversableOnce[MultivariablePolynomial[Int, V]], variables: Seq[V], boundary: Option[Map[V, Int] => Boolean] = None, knownSolution: Option[Map[V, Int]] = None): Iterator[Map[V, Int]] = {

    type P = MultivariablePolynomial[Int, V]
    val polynomialAlgebra: MultivariablePolynomialAlgebra[Int, V] = implicitly

    // this is a hack; caseBash could be designed differently so this isn't needed.
    lazy val cachedLimit = {
      val cacheKeys = Array.fill[Map[V, Int]](20)(null)
      val cacheValues = Array.fill(20)(false)
      var k = 0

      { m: Map[V, Int] =>
      	cacheKeys.indexOf(m) match {
      	  case -1 => {
      	    cacheKeys(k) = m
      	    val result = boundary.get(m)
      	    cacheValues(k) = result
      	    k = (k + 1) % 20      	
      	    result
      	  }
      	  case n => {
      	    cacheValues(n)
      	  }
      	}
      }
    }

    case class Equations(substitutions: Map[V, P], equations: Seq[P], caseBashProgress: Seq[(Int, Int)]) {
      //      if ((equations.nonEmpty && equations.size % 1000 == 0) || (substitutions.nonEmpty && substitutions.size % 1000 == 0)) info(substitutions.size + " " + equations.size)
      //      equations.headOption.map(info(_))
      //      require(equations.flatMap(_.variables).toSet.intersect(substitutions.keySet).isEmpty)

      //      if (equations.nonEmpty) {
      //        val e = equations.minBy(_.toString.size)
      //        if (e.totalDegree == Some(2) && e.terms.size == 1 && e.terms(0)._1.keys.size == 1) require(false)
      //        if (!mentioned.contains(e)) {
      //          if (e.totalDegree == Some(2) && e.terms.size == 1 && e.terms(0)._1.keys.size == 2) {
      //
      //          } else {
      //            println(e)
      //          }
      //          mentioned += e
      //        }
      //      }

      def addSubstitution(v: V, k: Int): Option[Equations] = {
        //        knownSolution map { solution =>
        //          require(solution.get(v).getOrElse(k) == k)
        //        }
        addSubstitution(v, polynomialAlgebra.constant(k))
      }
      def addSubstitution(v: V, p: P): Option[Equations] = {
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

          Equations(newSubstitutions + (v -> p), toKeep, caseBashProgress).addEquations(toReprocess.map(polynomialAlgebra.substitute(Map(v -> p))))
        }
      }
      def recordCaseBashProgress(caseIndex: Int, totalCases: Int) = copy(caseBashProgress = caseBashProgress :+ (caseIndex, totalCases))
      def addEquations(qs: TraversableOnce[P]): Option[Equations] = {
        qs.foldLeft[Option[Equations]](Some(this))({ (o, e) => o.flatMap(_.addEquation(e)) })
      }
      def addEquation(q: P): Option[Equations] = {
        val p = polynomialAlgebra.substitute(substitutions)(q).divideByCoefficientGCD

        def splitIfPositiveOrOtherwiseAdd: Option[Equations] = {
          if (p.terms.size > 1 && (p.terms.forall(_._2 > 0) || p.terms.forall(_._2 < 0))) {
            addEquations(p.terms.map(t => polynomialAlgebra.monomial(t._1)))
          } else {
            add
          }
        }

        def add: Some[Equations] = {
          if (p.terms.size > 1) {
            require(p.terms.exists(_._2 > 0) && p.terms.exists(_._2 < 0))
          }
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
            if (p.terms.size == 1) {
              val t = p.terms.head
              require(t._2 != 0)
              val v = t._1.keysIterator.next
              addSubstitution(v, 0)
            } else if (p.terms.size == 2) {
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
                val List(t1, t2) = p.terms.toList
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
            p.terms.size match {
              case 1 => {
                val h = p.terms(0)
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
                val List(t1, t2) = p.terms.toList.sortBy(_._1.values.sum)
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
                val List(t1, t2, t3) = p.terms.toList.sortBy(t => (t._1.values.sum, t._2))
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

      def solveALinearEquation: Option[Equations] = {
        def solve(p: P): Option[Equations] = {
          val t = p.termsOfDegree(1).find(_._2 == 1).get
          val v = t._1.keysIterator.next
          addSubstitution(v, polynomialAlgebra.subtract(polynomialAlgebra.monomial(v), p))
        }

        equations.find(p => p.totalDegree == Some(1) && p.termsOfDegree(1).exists(_._2 == 1) && p.terms.count(_._2 > 0) == 1) match {
          case Some(p) => solve(p)
          case None => {
            equations.find(p => p.totalDegree == Some(1) && p.termsOfDegree(1).exists(_._2 == -1) && p.terms.count(_._2 < 0) == 1) match {
              case Some(p) => solve(polynomialAlgebra.negate(p))
              case None => Some(this)
            }
          }
        }
      }
      def solveLinearEquations: Option[Equations] = {
        import net.tqft.toolkit.functions.FixedPoint
        FixedPoint({ o: Option[Equations] => o.flatMap(_.solveALinearEquation) })(Some(this))
      }

      def cases(v: V): Seq[Int] = {
        val remainingVariables = variables.filterNot(substitutions.keySet.contains).filterNot(_ == v)
        val limit = boundary.get

        val minimalSubstitutionBase = {
          val newSubstitutions = remainingVariables.map(w => w -> polynomialAlgebra.constant(0)).toMap
          substitutions.mapValues(p => polynomialAlgebra.substitute(newSubstitutions)(p)) ++ newSubstitutions
        }
        def minimalSubstitution(k: Int) = {
          minimalSubstitutionBase.mapValues(p => polynomialAlgebra.completelySubstituteConstants(Map(v -> k))(p)) + (v -> k)
        }
        def fasterMinimalSubstitution(k: Int) = {
          val newSubstitutions = ((v -> k) +: remainingVariables.map(w => w -> 0)).toMap
          substitutions.mapValues(p => polynomialAlgebra.completelySubstituteConstants(newSubstitutions)(p)) ++ newSubstitutions
        }

        Iterator.from(0).takeWhile({ k => cachedLimit(fasterMinimalSubstitution(k)) }).toSeq
      }

      def caseBashOneStep(v: V): Iterator[Equations] = {
        val c = cases(v)
        (for (k <- c.iterator; r <- recordCaseBashProgress(k, c.size).addSubstitution(v, k).flatMap(_.solveLinearEquations)) yield {
//          info("case bashing " + v + " via " + r.caseBashProgress.map(p => (p._1 + 1) + "/" + p._2).mkString(", ") + " cases, " + r.equations.size + " remaining equations")
          r
        })
      }

      // TODO if there are no more equations, there's no point choosing the smallest fork
      def caseBash: Iterator[Equations] = {
        val remainingVariables = variables.filterNot(substitutions.keySet.contains)
        if (remainingVariables.nonEmpty) {
          val v = if (equations.nonEmpty) {
            remainingVariables.minBy(v => cases(v).size)
          } else {
            remainingVariables.head
          }
          (for (c1 <- caseBashOneStep(v); c2 <- c1.caseBash) yield c2)
        } else {
          Iterator(this)
        }
      }
    }

    val iterator = Equations(Map.empty, Seq.empty, Seq.empty).addEquations(polynomials).flatMap(_.solveLinearEquations) match {
      case None => {
        Iterator.empty
      }
      case Some(equations) => {
        equations.caseBash
      }
    }

    iterator.map(_.substitutions.mapValues(p => p.ensuring(_.totalDegree.getOrElse(0) == 0).constantTerm))
  }
}