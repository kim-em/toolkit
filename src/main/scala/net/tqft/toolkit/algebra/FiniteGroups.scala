package net.tqft.toolkit.algebra

trait FiniteGroup[A] extends Group[A] {
  def elements: Set[A]
  def size: Int = elements.size

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

  def generatedBy[A](group: FiniteGroup[A], subgroups: Set[FiniteGroup[A]]): FiniteGroup[A] = {
    val subgroupElements = subgroups.flatMap { sg: FiniteGroup[A] => sg.elements }
    import net.tqft.toolkit.functions.FixedPoint
    val elements = FixedPoint({ s: Set[A] => s flatMap { x => subgroupElements map { group.multiply(_, x) } } })(subgroupElements)
    subgroup(group, elements)
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
  
  implicit def equivalenceClassOrdering[A](implicit o: Ordering[A]): Ordering[EquivalenceClass[A]] = new Ordering[EquivalenceClass[A]] {
    def compare(x: EquivalenceClass[A], y: EquivalenceClass[A]) = o.compare(x.leastRepresentative, y.leastRepresentative)
  }
  
  trait LeftCoset[A] extends EquivalenceClass[A] {
    def group: FiniteGroup[A]
    def stabilizer: FiniteGroup[A]

    override  def contains(a: A) = stabilizer.elements.contains(group.multiply(group.inverse(a), representative))

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

  def subgroup[A](group: FiniteGroup[A], _elements: Set[A]) = {
    new FiniteGroup[A] {
      def one = group.one
      def inverse(a: A) = group.inverse(a)
      def multiply(a: A, b: A) = group.multiply(a, b)

      def elements = _elements
    }
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

}