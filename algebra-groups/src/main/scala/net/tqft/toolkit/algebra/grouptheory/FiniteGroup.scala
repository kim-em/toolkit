package net.tqft.toolkit.algebra.grouptheory

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.polynomials._
import net.tqft.toolkit.algebra.matrices._
import net.tqft.toolkit.algebra.numberfields._
import scala.collection.GenSet
import scala.collection.GenSeq
import scala.language.implicitConversions
import net.tqft.toolkit.Logging

object FiniteGroup extends net.tqft.toolkit.Logging

trait FiniteGroup[A] extends Group[A] with Finite[A] { finiteGroup =>

  def verifyInverses = {
    for (x <- elements) {
      require(multiply(x, inverse(x)) == one)
      require(multiply(inverse(x), x) == one)
    }
    true
  }

  def verifyAssociativity = {
    for (x <- elements; y <- elements; z <- elements) {
      require(multiply(multiply(x, y), z) == multiply(x, multiply(y, z)))
    }
    true
  }

  def verifySubgroup(subgroup: Set[A]) = {
    for (x <- subgroup) {
      require(subgroup.contains(inverse(x)))
    }
    for (x <- subgroup; y <- subgroup) {
      require(subgroup.contains(multiply(x, y)))
    }
    true
  }

  def verifyNormalSubgroup(subgroup: Set[A]) = {
    verifySubgroup(subgroup)
    for (x <- subgroup; y <- elements) {
      val p = multiply(multiply(inverse(y), x), y)
      require(subgroup.contains(p))
    }
    true
  }

  override def elements: Set[A]
  
  trait Action[B] { action =>
    def act(a: A, b: B): B
    def orbits(elements: Set[B]): Set[Orbit] = bruteForceOrbits(finiteGroup.elements, elements)
    
    type Orbit =  net.tqft.toolkit.algebra.grouptheory.Orbit[A, B]
    
    protected def bruteForceOrbits(generators: Set[A], elements: Set[B]) = {
      class O(val representative: B) extends Orbit {
        override def stabilizer = ???
        override lazy val elements = extendElements(Seq.empty, Seq(representative)).toSet

        override def hashCode = elements.hashCode
        override def equals(other: Any) = {
          other match {
            case other: Orbit => other.elements == elements
            case _ => false
          }
        }

        @scala.annotation.tailrec
        private def extendElements(elements: Seq[B], newestElements: GenSeq[B]): Seq[B] = {
          if (newestElements.isEmpty) {
            elements
          } else {
            val allElements = (elements ++ newestElements).distinct;
            extendElements(allElements, (for (b <- newestElements; a <- generators) yield act(a, b)).distinct.filterNot(allElements.contains))
          }
        }
      }

      @scala.annotation.tailrec
      def extractOrbits(objects: Set[B], orbits: Set[Orbit]): Set[Orbit] = {
        if (objects.isEmpty) {
          orbits
        } else {
          val newOrbit = new O(objects.head)
          extractOrbits(objects diff newOrbit.elements, orbits + newOrbit)
        }
      }

      extractOrbits(elements, Set())
    }

  }

  def trivialAction[B] = new Action[B] {
    override def act(a: A, b: B) = b
    override def orbits(set: Set[B]) = set.map(b => Orbit.singleton(finiteGroup, b))
  }

  protected class ConjugationAction extends Action[A] {
    override def act(a: A, b: A) = multiply(inverse(a), b, a)
  }
  
  def conjugationAction = new ConjugationAction
  
  private trait Subgroup extends FiniteGroup[A] {
    override def one = finiteGroup.one
    override def inverse(a: A) = finiteGroup.inverse(a)
    override def multiply(a: A, b: A) = finiteGroup.multiply(a, b)

    override def hashCode = elements.hashCode + 17
    override def equals(other: Any) = {
      other match {
        case other: finiteGroup.Subgroup => elements == other.elements
        case _ => false
      }
    }
  }

  private class FinitelyGeneratedSubgroup(val generators: Seq[A]) extends Subgroup with FinitelyGeneratedFiniteGroup[A] {
  }

  def subgroup(elements: Set[A]): FiniteGroup[A] = {
    val _elements = elements
    new Subgroup {
      override val elements = _elements
    }
  }
  def subgroupGeneratedBy(generators: Seq[A]): FinitelyGeneratedFiniteGroup[A] = new FinitelyGeneratedSubgroup(generators)

  def subgroups: Set[FiniteGroup[A]] = {
    def build(G: FinitelyGeneratedSubgroup, elts: List[A]): Set[FinitelyGeneratedSubgroup] = {
      (for (i <- 0 until elts.size; e = elts(i); h = new FinitelyGeneratedSubgroup(G.generators :+ e); k <- build(h, elts.drop(i + 1).filterNot(h.elements))) yield k).toSet + G
    }
    build(new FinitelyGeneratedSubgroup(Seq.empty), (elements - one).toList).asInstanceOf[Set[FiniteGroup[A]]]
  }
  def subgroupsUpToConjugacy: Set[FiniteGroup[A]] = {
    val action = new Action[Subgroup] {
      override def act(a: A, G: Subgroup) = new Subgroup {
        override val elements = G.elements.map(g => finiteGroup.multiply(finiteGroup.inverse(a), g, a))
      }
    }
    action.orbits(subgroups.asInstanceOf[Set[Subgroup]]).map(_.representative.asInstanceOf[FiniteGroup[A]])
  }

  protected def unsortedConjugacyClasses = finiteGroup.conjugationAction.orbits(elements).toSeq

  lazy val conjugacyClasses = unsortedConjugacyClasses.sortBy({ c => (c.representative != one, c.elements.size) })
  lazy val conjugacyClassOrders = for (c <- conjugacyClasses) yield orderOfElement(c.representative)
  private def exponentationOnConjugacyClassesImpl(k: Int) = conjugacyClasses.map({ cx => conjugacyClasses.indexWhere({ cy => cy.elements.contains(power(cx.representative, k)) }) }).toIndexedSeq
  private lazy val primeExponentiationOnConjugacyClasses: (Int => IndexedSeq[Int]) = {
    import net.tqft.toolkit.arithmetic.Primes
    Primes.takeWhile({ n => n < exponent }).map({ n => n -> exponentationOnConjugacyClassesImpl(n) }).toMap
  }
  lazy val exponentiationOnConjugacyClasses: Int => IndexedSeq[Int] = {
    def impl(k: Int) = {
      if (k == 0) {
        IndexedSeq.fill(conjugacyClasses.size)(0)
      } else {
        import net.tqft.toolkit.arithmetic.Factor
        import net.tqft.toolkit.permutations.Permutations._
        Factor(k).map(primeExponentiationOnConjugacyClasses(_)).fold(0 until conjugacyClasses.size)({ (p: IndexedSeq[Int], q: IndexedSeq[Int]) => p permute q })
      }
    }
    (for (n <- 0 until exponent) yield n -> impl(n)).toMap
  }
  lazy val inverseOnConjugacyClasses = exponentationOnConjugacyClassesImpl(-1)
  lazy val inverseConjugacyClasses = {
    import net.tqft.toolkit.permutations.Permutations._
    inverseOnConjugacyClasses permute conjugacyClasses
  }

  val classCoefficients: Int => Seq[Seq[Int]] = {
    def impl(ix: Int) = {
      val cx = inverseConjugacyClasses(ix)
      (for (p <- conjugacyClasses.zipWithIndex.par) yield {
        val (cy, iy) = p
        FiniteGroup.info("Computing class coefficient " + (ix, iy))
        for (cz <- conjugacyClasses; z = cz.representative) yield {
          cx.elements.count({ x => cy.elements.contains(multiply(x, z)) })
        }
      }).seq
    }
    import net.tqft.toolkit.functions.Memo._
    (impl _).memo
  }

  lazy val exponent = Integers.lcm((elements map { orderOfElement _ }).toSeq: _*)

  lazy val preferredPrime = {
    var p = exponent + 1
    while ((p * p <= 4 * size) || !BigInt(p).isProbablePrime(60)) p = p + exponent
    p
  }

  trait Character[F] {
    def field: Field[F]
    def character: Seq[F]
    def degree: Int
  }
  object Character {
    //    import language.implicitConversions
    implicit def liftRationals(x: Seq[Fraction[BigInt]]): RationalCharacter = new RationalCharacter {
      override val character = x
    }
    implicit def liftIntegers(x: Seq[Int]): RationalCharacter = new RationalCharacter {
      override val character = x.map(y => Fraction.whole(BigInt(y)))
    }
  }

  trait RationalCharacter extends Character[Fraction[BigInt]] {
    override def field = implicitly[Field[Fraction[BigInt]]]
    override def degree = character.head.ensuring(_.denominator == 1).numerator.toInt
  }
  trait CyclotomicCharacter extends Character[Polynomial[Fraction[BigInt]]] {
    def order: Int
    override def field = NumberField.cyclotomic(order)
    override def degree = character.head.ensuring(_.maximumDegree.getOrElse(0) == 0).constantTerm.ensuring(_.denominator == BigInt(1)).numerator.toInt
  }

  // This implementation of the Burnside-Dixon algorithm follows the description at http://www.maths.qmul.ac.uk/~rtb/mathpage/Richard%20Bayley's%20Homepage_files/Dixon.pdf
  lazy val characters: Seq[Seq[Polynomial[Fraction[BigInt]]]] = {
    val k = conjugacyClasses.size

    def classCoefficientSimultaneousEigenvectorsModPrime = {
      import net.tqft.toolkit.arithmetic.Mod._
      implicit val modP = PrimeField(preferredPrime)
      val zeroVector = Seq.fill(k)(0)

      def subtractDiagonal(m: Seq[Seq[Int]], lambda: Int) = {
        m.zipWithIndex.map({ case (r, i) => r.updated(i, modP.subtract(r(i), lambda)) })
      }

      def eigenvalues(m: Seq[Seq[Int]]) = Matrix(m.size, m.par).eigenvalues

      case class PartialEigenspace(annihilators: GenSeq[Seq[Int]], eigenvalues: Seq[Int], eigenvectors: Option[Seq[Seq[Int]]]) {
        def splitAlong(m: => Seq[Seq[Int]], mEigenvalues: => Set[Int]): Set[PartialEigenspace] = {
          eigenvectors.map(_.size) match {
            case Some(0) => Set()
            case Some(1) => {
              Set(this)
            }
            case _ => {
              for (lambda <- mEigenvalues) yield {
                val newAnnihilators = Matrix(conjugacyClasses.size, annihilators ++ subtractDiagonal(m, lambda)).rowEchelonForm(modP)
                PartialEigenspace(newAnnihilators.entries, eigenvalues :+ lambda, Some(newAnnihilators.nullSpace))
              }
            }
          }
        }
      }

      val cachedEigenvalues = {
        def impl(n: Int) = Matrix(k, classCoefficients(n).par).eigenvalues
        import net.tqft.toolkit.functions.Memo._
        (impl _).memo
      }

      val unnormalizedEigenvectors = (1 until k).foldLeft(Set(PartialEigenspace(Seq().par, Seq(), None)))({ case (s, i) => s.flatMap({ p => p.splitAlong(classCoefficients(i), cachedEigenvalues(i)) }) }).flatMap(_.eigenvectors.get).toSeq
      FiniteGroup.info("Found simultaneous eigenvectors.")

      val normalizedEigenvectors = unnormalizedEigenvectors.map({ v => v.map({ x => modP.quotient(x, v(0)) }) })

      normalizedEigenvectors
    }

    def characterTableModPreferredPrime = {
      implicit val modP = PrimeField(preferredPrime)

      def sqrt(x: Int) = {
        import net.tqft.toolkit.arithmetic.Mod._
        (0 to preferredPrime / 2).find({ n => ((n * n - x) mod preferredPrime) == 0 }).get
      }

      val omega = classCoefficientSimultaneousEigenvectorsModPrime

      println("omega -> " + omega)

      val degrees = for (omega_i <- omega) yield {
        sqrt(modP.quotient(finiteGroup.size, (for (j <- 0 until k) yield modP.quotient(omega_i(j) * omega_i(inverseOnConjugacyClasses(j)), conjugacyClasses(j).size)).sum))
      }

      println("degrees -> " + degrees)
      require(degrees.map(n => n * n).sum == size)

      for (i <- 0 until k) yield for (j <- 0 until k) yield modP.quotient(omega(i)(j) * degrees(i), conjugacyClasses(j).size)
    }

    val modP = PrimeField(preferredPrime)

    val cyclotomicNumbers = NumberField.cyclotomic[BigInt](exponent)
    val zeta = Polynomial.identity[Fraction[BigInt]]
    val chi = characterTableModPreferredPrime
    println("preferredPrime -> " + preferredPrime)
    println("chi -> " + chi)

    val z = (1 until preferredPrime).find({ n => modP.orderOfElement(n) == exponent }).get

    val zpower = IndexedSeq.tabulate(exponent)({ k: Int => modP.power(z, k) })
    val zetapower = IndexedSeq.tabulate(exponent)({ k: Int => cyclotomicNumbers.power(zeta, k) })

    import net.tqft.toolkit.arithmetic.Mod._

    // ACHTUNG!
    // make sure we don't deadlock (c.f. https://issues.scala-lang.org/browse/SI-5808)
    exponentiationOnConjugacyClasses(0)

    def mu(i: Int)(j: Int)(s: Int) = modP.quotient(modP.sum(for (n <- 0 until exponent) yield modP.multiply(chi(i)(exponentiationOnConjugacyClasses(n)(j)), zpower((-s * n) mod exponent))), exponent)

    val unsortedCharacters = (for (i <- (0 until k).par) yield (for (j <- (0 until k).par) yield {
      FiniteGroup.info("Computing entry " + (i, j) + " in the character table.")
      cyclotomicNumbers.sum(for (s <- 0 until exponent) yield cyclotomicNumbers.multiplyByInt(zetapower(s), mu(i)(j)(s)))
    }).seq).seq
    val sortedCharacters = {
      implicit val rationals = implicitly[OrderedField[Fraction[BigInt]]]
      unsortedCharacters.sortBy({ v => (v(0).constantTerm, v.tail.headOption.map({ p => rationals.negate(p.constantTerm) })) })
    }

    sortedCharacters
  }

  def reducedCharacters: Seq[Character[_]] = {
    for (c <- characters) yield {
      if (c.map(_.maximumDegree.getOrElse(0)).max == 0) {
        new RationalCharacter {
          val character = c.map(_.constantTerm)
        }
      } else {
        // TODO really should find the smallest cyclotomic field
        // see http://www.math.ru.nl/~bosma/pubs/AAECC1990.pdf
        new CyclotomicCharacter {
          val order = exponent
          val character = c
        }
      }
    }
  }
  def rationalCharacters: Seq[RationalCharacter] = reducedCharacters collect { case c: RationalCharacter => c }

  def characterPairing[M, N](m: Character[M], n: Character[N]): Int = {
    def liftCharacterToCyclotomicFieldOfExponent(c: Character[_]) = {
      val polynomials = implicitly[Polynomials[Fraction[BigInt]]]
      new CyclotomicCharacter {
        override val order = exponent
        override val character = c match {
          case c: RationalCharacter => c.character.map({ x => Polynomial.constant(x) })
          case c: CyclotomicCharacter => {
            val power = exponent / c.order
            c.character.map({ p => field.normalForm(polynomials.composeAsFunctions(p, polynomials.monomial(power))) })
          }
        }
      }
    }
    val Q = NumberField.cyclotomic[BigInt](exponent)
    val result = Q.quotientByInt(
      Q.sum(
        for (((a, b), t) <- liftCharacterToCyclotomicFieldOfExponent(m).character zip liftCharacterToCyclotomicFieldOfExponent(n).character zip conjugacyClasses.map(_.size)) yield {
          Q.multiplyByInt(Q.multiply(a, Q.bar(b)), t)
        }),
      finiteGroup.size)
    require(result.maximumDegree.getOrElse(0) == 0)
    val rationalResult = result.constantTerm
    require(rationalResult.denominator == BigInt(1))
    rationalResult.numerator.toInt
  }

  def verifyOrthogonalityOfCharacters = {
    reducedCharacters.forall(c => characterPairing(c, c) == 1) &&
      (for (i <- (0 until conjugacyClasses.size).iterator; c1 = reducedCharacters(i); j <- 0 until i; c2 = reducedCharacters(j)) yield characterPairing(c1, c2) == 0).forall(_ == true)
  }

  // TODO rewrite this in terms of other stuff!
  lazy val tensorProductMultiplicities: Seq[Seq[Seq[Int]]] = {
    val k = conjugacyClasses.size
    implicit val Q = NumberField.cyclotomic[BigInt](exponent)

    def pairing(x: Seq[Polynomial[Fraction[BigInt]]], y: Seq[Polynomial[Fraction[BigInt]]]): Polynomial[Fraction[BigInt]] = {
      Q.quotientByInt(Q.sum((x zip y zip conjugacyClasses.map(_.size)).map({ p => Q.multiplyByInt(Q.multiply(p._1._1, p._1._2), p._2) })), finiteGroup.size)
    }

    def lower(p: Polynomial[Fraction[BigInt]]): Int = {
      if (p == Q.zero || p.maximumDegree == Some(0)) {
        val c = p.constantTerm
        require(c.denominator == 1)
        c.numerator.toInt
      } else {
        Logging.warn("Something went wrong computing a pairing between characters: not an integer!")
        ???
      }
    }

    // hmm, deadlocks; maybe this will make sure they don't happen!
    characters(0)

    (for (i <- (0 until k).par) yield {
      FiniteGroup.info("Computing tensor product multiplicities (" + i + ", *)")
      (for (j <- (0 until k).par) yield {
        val product = (characters(i) zip characters(j)).map({ x => Q.multiply(x._1, x._2) }) map (Q.bar _)
        for (c <- characters) yield lower(pairing(product, c))
      }).seq
    }).seq
  }
}

