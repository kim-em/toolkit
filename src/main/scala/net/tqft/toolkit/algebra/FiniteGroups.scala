package net.tqft.toolkit.algebra

trait FiniteGroup[A] extends Group[A] with Elements[A] { finiteGroup =>
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

  private class Subgroup(val elements: Set[A]) extends FiniteGroup[A] {
    def one = finiteGroup.one
    def inverse(a: A) = finiteGroup.inverse(a)
    def multiply(a: A, b: A) = finiteGroup.multiply(a, b)
  }
  private class FinitelyGeneratedSubgroup(val generators: Set[A]) extends Subgroup({
    import net.tqft.toolkit.functions.FixedPoint
    FixedPoint({ s: Set[A] => s flatMap { x => generators map { multiply(_, x) } } })(Set(one))
  }) with FinitelyGeneratedFiniteGroup[A]

  def subgroup(elements: Set[A]): FiniteGroup[A] = new Subgroup(elements)
  def subgroupGeneratedBy(generators: Set[A]): FinitelyGeneratedFiniteGroup[A] = new FinitelyGeneratedSubgroup(generators)

  lazy val conjugacyClasses = GroupActions.conjugationAction(finiteGroup).orbits(elements, elements).toSeq.sortBy({ c => (c.representative != one, c.elements.size) })
  lazy val conjugacyClassOrders = for (c <- conjugacyClasses) yield orderOfElement(c.representative)
  lazy val inverseOnConjugacyClasses: Seq[Int] = conjugacyClasses.map({ cx => conjugacyClasses.indexWhere({ cy => cy.elements.contains(inverse(cx.representative)) }) })
  def inverseConjugacyClasses = {
    import net.tqft.toolkit.permutations.Permutations.Permutation2RichPermutation
    inverseOnConjugacyClasses permute conjugacyClasses
  }

  lazy val classCoefficients: Seq[Seq[Seq[Int]]] = {
    for ((cx, ix) <- inverseConjugacyClasses.zipWithIndex) yield {
      (for ((cy, iy) <- conjugacyClasses.zipWithIndex.par) yield {
        println((ix, iy))
        for (cz <- conjugacyClasses; z = cz.representative) yield {
          cx.elements.count({ x => cy.elements.contains(multiply(x, z)) })
        }
      }).seq
    }
  }

  def orderOfElement(a: A): Int = Iterator.iterate(a)(multiply(_, a)).indexOf(one) + 1
  lazy val exponent = Gadgets.Integers.lcm((elements map { orderOfElement _ }).toSeq: _*)

  lazy val preferredPrime = {
    var p = exponent + 1
    while (!BigInt(p).isProbablePrime(60)) p = p + exponent
    p
  }

  lazy val classCoefficientSimultaneousEigenvectorsModPrime = {
    import net.tqft.toolkit.arithmetic.Mod._
    implicit val modP = Mod(preferredPrime)
    val zeroVector = List.fill(conjugacyClasses.size)(0).toSeq

    def subtractDiagonal(m: Seq[Seq[Int]], lambda: Int) = {
      m.zipWithIndex.map({ case (r, i) => r.updated(i, (r(i) - lambda) mod preferredPrime) })
    }

    def eigenvalues(m: Seq[Seq[Int]]) = new Matrix(m.size, m).eigenvalues

    case class PartialEigenspace(annihilators: Seq[Seq[Seq[Int]]], eigenvalues: Seq[Int], eigenvectors: Option[Seq[Seq[Int]]]) {
      def splitAlong(m: Seq[Seq[Int]], mEigenvalues: Set[Int]): Set[PartialEigenspace] = {
        println(annihilators)
        println(eigenvalues)
        println(eigenvectors)
        println("splitting along: " + m)
        println("splitting along eigenvalues: " + mEigenvalues)
        eigenvectors.map(_.size) match {
          case Some(0) => Set()
          case Some(1) => {
            val newEigenvalue: Int = mEigenvalues.find({ x => new Matrix(conjugacyClasses.size, subtractDiagonal(m, x)).apply(eigenvectors.get.head).map(_ mod preferredPrime) == zeroVector }).get
            Set(PartialEigenspace(annihilators :+ subtractDiagonal(m, newEigenvalue), eigenvalues :+ newEigenvalue, eigenvectors))
          }
          case _ => {
            for (lambda <- mEigenvalues) yield {
              val newAnnihilators = annihilators :+ subtractDiagonal(m, lambda)
              PartialEigenspace(newAnnihilators, eigenvalues :+ lambda, Some(new Matrix(conjugacyClasses.size, newAnnihilators.flatten.toSeq).nullSpace))
            }
          }
        }
      }
    }

    val unnormalizedEigenvectors = classCoefficients.par.map({ cc => (cc, eigenvalues(cc)) }).foldLeft(Set(PartialEigenspace(Seq(), Seq(), None)).par)({ case (s, (cc, ev)) => s.flatMap({ p => p.splitAlong(cc, ev) }) }).seq.flatMap(_.eigenvectors.get).toSeq
    unnormalizedEigenvectors.map({v => v.map({ x => modP.quotient(x, v(0))})})
  }

  lazy val characterTableModPreferredPrime = {
        implicit val modP = Mod(preferredPrime)

    
    def sqrtModPrime(x: Int) = {
      import net.tqft.toolkit.arithmetic.Mod._
      (0 until preferredPrime / 2).find({ n => ((n * n - x) mod preferredPrime) == 0 }).get
    }

    val omega = classCoefficientSimultaneousEigenvectorsModPrime
    val degrees = for (omega_i <- omega) yield {
      sqrtModPrime(modP.quotient(finiteGroup.size, (for (j <- 0 until conjugacyClasses.size) yield modP.quotient(omega_i(j) * omega_i(inverseOnConjugacyClasses(j)), conjugacyClassOrders(j))).sum))
    }
    
    degrees//.map({n => n*n}).sum
  }

}

trait FinitelyGeneratedFiniteGroup[A] extends FiniteGroup[A] { fgFiniteGroup =>
  def generators: Set[A]
  override lazy val conjugacyClasses = GroupActions.conjugationAction(fgFiniteGroup).orbits(generators, elements).toSeq
}

trait EquivalenceClass[A] {
  def representative: A
  def elements: Set[A]
  def contains(a: A) = elements.contains(a)

  def leastRepresentative(implicit o: Ordering[A]) = elements.min

  override def equals(other: Any) = {
    other match {
      case other: EquivalenceClass[_] => {
        contains(other.asInstanceOf[EquivalenceClass[A]].representative)
      }
      case _ => false
    }
  }
  override def hashCode = {
    elements.hashCode
  }
}

object EquivalenceClass {
  implicit def equivalenceClassOrdering[A](implicit o: Ordering[A]): Ordering[EquivalenceClass[A]] = new Ordering[EquivalenceClass[A]] {
    def compare(x: EquivalenceClass[A], y: EquivalenceClass[A]) = o.compare(x.leastRepresentative, y.leastRepresentative)
  }
}

object FiniteGroups {

  def trivialGroup[A](one: A): FiniteGroup[A] = new TrivialGroup(one)
  def cyclicGroup(n: Int): FiniteGroup[Int] = new CyclicGroup(n)
  def dihedralGroup(n: Int): FiniteGroup[(Int, Boolean)] = {
    if (n == 0) {
      new TrivialGroup((0, false))
    } else {
      new DihedralGroup(n)
    }
  }

  trait LeftCoset[A] extends EquivalenceClass[A] {
    def group: FiniteGroup[A]
    def stabilizer: FiniteGroup[A]

    override def contains(a: A) = stabilizer.elements.contains(group.multiply(group.inverse(a), representative))

    def elements = (stabilizer.elements.map { group.multiply(representative, _) })
    override def toString = elements.mkString("LeftCoset[", ", ", "]")
  }

  trait DoubleCoset[A] extends EquivalenceClass[A] {
    def group: FiniteGroup[A]
    def leftStabilizer: FiniteGroup[A]
    def rightStabilizer: FiniteGroup[A]

    // TODO is there a better implementation of contains than the one inherited from EquivalenceClass? 
    // def contains(a: A) = elements.contains(a)

    def elements = for (h <- leftStabilizer.elements; g <- rightStabilizer.elements; x = representative) yield group.multiply(h, group.multiply(x, g))
    override def toString = elements.mkString("DoubleCoset[", ", ", "]")
  }

  def leftCosets[A](_group: FiniteGroup[A], subgroup: FiniteGroup[A]): Set[LeftCoset[A]] = {
    class C(val representative: A) extends LeftCoset[A] {
      def group = _group
      def stabilizer = subgroup
    }

    def extractCosets(elements: Set[A], cosets: Set[LeftCoset[A]]): Set[LeftCoset[A]] = {
      if (elements.isEmpty) {
        cosets
      } else {
        val newCoset = new C(elements.head)
        extractCosets(elements -- newCoset.elements, cosets + newCoset)
      }
    }
    val result = extractCosets(_group.elements, Set())
    result ensuring { _.size == _group.elements.size / subgroup.elements.size }
  }

  def doubleCosets[A](_group: FiniteGroup[A], leftSubgroup: FiniteGroup[A], rightSubgroup: FiniteGroup[A]): Set[DoubleCoset[A]] = {
    class D(val representative: A) extends DoubleCoset[A] {
      def group = _group
      def leftStabilizer = leftSubgroup
      def rightStabilizer = rightSubgroup
    }

    def extractCosets(elements: Set[A], cosets: Set[DoubleCoset[A]]): Set[DoubleCoset[A]] = {
      if (elements.isEmpty) {
        cosets
      } else {
        val newCoset = new D(elements.head)
        extractCosets(elements -- newCoset.elements, cosets + newCoset)
      }
    }
    extractCosets(_group.elements, Set())
  }

  def quotient[A](_group: FiniteGroup[A], normalSubgroup: FiniteGroup[A]): FiniteGroup[LeftCoset[A]] = {
    class C(val representative: A) extends LeftCoset[A] {
      def group = _group
      def stabilizer = normalSubgroup
    }

    new FiniteGroup[LeftCoset[A]] {
      def one = new C(_group.one)
      def inverse(a: LeftCoset[A]) = new C(_group.inverse(a.representative))
      def multiply(a: LeftCoset[A], b: LeftCoset[A]) = new C(_group.multiply(a.representative, b.representative))

      def elements = leftCosets(_group, normalSubgroup)
    }
  }

  private class TrivialGroup[A](identity: A) extends FiniteGroup[A] {
    def one = identity
    def inverse(a: A) = identity
    def multiply(a: A, b: A) = identity

    val elements = Set(identity)
  }

  // this is the dihedral group with 2*n elements
  private class DihedralGroup(n: Int) extends FiniteGroup[(Int, Boolean)] {
    require(n > 0)

    import net.tqft.toolkit.arithmetic.Mod._

    override def one = (0, false)
    override def inverse(a: (Int, Boolean)) = (if (a._2) a._1 else (-a._1 mod n), a._2)
    //    override def multiply(a: (Int, Boolean), b: (Int, Boolean)) = ((b._1 + (if (b._2) -a._1 else a._1)) mod n, a._2 ^ b._2)
    override def multiply(a: (Int, Boolean), b: (Int, Boolean)) = ((a._1 + (if (a._2) -b._1 else b._1)) mod n, a._2 ^ b._2)

    override val elements = (for (j <- Set(false, true); i <- 0 until n) yield (i, j))
  }
  private class CyclicGroup(n: Int) extends FiniteGroup[Int] {
    import net.tqft.toolkit.arithmetic.Mod._

    override def one = 0
    override def inverse(a: Int) = -a mod n
    override def multiply(a: Int, b: Int) = (a + b) mod n

    override val elements = (0 until n).toSet
  }
  private class PermutationGroup(n: Int) extends FiniteGroup[Seq[Int]] {
    import net.tqft.toolkit.permutations.Permutations
    import net.tqft.toolkit.permutations.Permutations.Permutation2RichPermutation
    def elements = Permutations.of(n).toSet
    def inverse(x: Seq[Int]) = Permutations.inverse(x)
    def multiply(x: Seq[Int], y: Seq[Int]) = x permute y
    def one = 0 until n

  }

  def permutationGroup(n: Int): FiniteGroup[Seq[Int]] = new PermutationGroup(n)
  def signedPermutationGroup(n: Int): FiniteGroup[(Seq[Int], Seq[Int])] = semidirectProduct(permutationGroup(n), power(cyclicGroup(2), n), GroupActions.permutationAction[Int])

  lazy val Mathieu12 = permutationGroup(12).subgroupGeneratedBy(Set(Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 0, 11), Seq(0, 1, 6, 9, 5, 3, 10, 2, 8, 4, 7, 11), Seq(11, 10, 5, 7, 8, 2, 9, 3, 4, 6, 1, 0)))

  private class SignedPermutationGroup(n: Int) extends FiniteGroup[(Seq[Int], Seq[Boolean])] {
    def elements = ???
    def inverse(x: (Seq[Int], Seq[Boolean])) = ???
    def multiply(x: (Seq[Int], Seq[Boolean]), y: (Seq[Int], Seq[Boolean])) = ???
    def one = (0 until n, List.fill(n)(true))
  }

  def semidirectProduct[A, B](group1: FiniteGroup[A], group2: FiniteGroup[B], action: GroupAction[A, B]): FiniteGroup[(A, B)] = {
    new FiniteGroup[(A, B)] {
      def elements = for (g1 <- group1.elements; g2 <- group2.elements) yield (g1, g2)
      def one = (group1.one, group2.one)
      def inverse(x: (A, B)) = ???
      def multiply(x: (A, B), y: (A, B)) = ???
    }
  }

  def product[A, B](group1: FiniteGroup[A], group2: FiniteGroup[B]) = semidirectProduct(group1, group2, GroupActions.trivialAction)

  def power[A](group: FiniteGroup[A], k: Int): FiniteGroup[Seq[A]] = {
    new FiniteGroup[Seq[A]] {
      def elements = ???
      def one = List.fill(k)(group.one)
      def inverse(x: Seq[A]) = x.map(group.inverse _)
      def multiply(x: Seq[A], y: Seq[A]) = (x zip y).map({ case (xg, yg) => group.multiply(xg, yg) })
    }
  }
}

trait GroupAction[A, B] {
  def act(a: A, b: B): B
  def orbits(generators: Set[A], objects: Set[B]): Set[Orbit[A, B]] = {
    class O(val representative: B) extends Orbit[A, B] {
      override def stabilizer = ???
      override lazy val elements = extendElements(Set(), Set(representative))

      @scala.annotation.tailrec
      private def extendElements(elements: Set[B], newestElements: Set[B]): Set[B] = {
        if (newestElements.isEmpty) {
          elements
        } else {
          val allElements = elements ++ newestElements;
          extendElements(allElements, (for (a <- generators; b <- newestElements) yield act(a, b)) -- allElements)
        }
      }
    }

    def extractOrbits(objects: Set[B], orbits: Set[Orbit[A, B]]): Set[Orbit[A, B]] = {
      if (objects.isEmpty) {
        orbits
      } else {
        val newOrbit = new O(objects.head)
        extractOrbits(objects -- newOrbit.elements, orbits + newOrbit)
      }
    }

    extractOrbits(objects, Set())
  }
}

trait Orbit[A, B] extends EquivalenceClass[B] {
  def stabilizer: FiniteGroup[A]
}

object GroupActions {
  def trivialAction[A, B]: GroupAction[A, B] = new GroupAction[A, B] {
    def act(a: A, b: B) = b
  }
  def permutationAction[C]: GroupAction[Seq[Int], Seq[C]] = new GroupAction[Seq[Int], Seq[C]] {
    import net.tqft.toolkit.permutations.Permutations.Permutation2RichPermutation

    def act(a: Seq[Int], b: Seq[C]) = a permute b
  }
  def conjugationAction[A](group: Group[A]) = new GroupAction[A, A] {
    def act(a: A, b: A) = group.multiply(group.inverse(a), b, a)
  }
}
