package net.tqft.toolkit.algebra.enumeration

trait EquivalenceClass[X] {
  def representative: X
  def contains(x: Any): Boolean // type parameter here ought to be x: X, but I ran into <https://issues.scala-lang.org/browse/SI-6522>
}

trait Group[G] {
  // a trait to describe an action of the group on a set of Xs
  trait Action[X] {
    def act(g: G)(x: X): X
    def elements: Set[X]

    def orbits: Set[EquivalenceClass[X]]
  }
}

trait FinitelyGeneratedGroup[G] extends Group[G] {
  def generators: Set[G]

  // when we have generators for the group we can calculate the orbits
  trait Action[X] extends super.Action[X] {
    override def orbits = ???
  }
}

// this line is a bit opaque... it says:
//   G represents elements of the automorphism group
//   B represents a value of a complete invariant (possible lazily evaluating)
//   A represents an eventual concrete realization of this trait, e.g. a TriangleFreeGraph
trait CanonicalGeneration[A <: CanonicalGeneration[A, G, B], G, B] { this: A =>
  val automorphisms: Group[G]

  implicit val ordering: Ordering[B]
  def invariant: B

  // in each problem instance, we will specify what the upper and lower objects actually look like
  type Lower <: {
    val result: A
  }
  type Upper <: {
    val result: A
    def inverse: result.Lower
  }

  // and generate them, along with an action of automorphisms
  def upperObjects: automorphisms.Action[Upper]
  def lowerObjects: automorphisms.Action[Lower]

  // now the actual algorithm
  def children = {
    for (
      orbit <- upperObjects.orbits;
      candidateUpperObject = orbit.representative;
      canonicalReductionOrbit = candidateUpperObject.result.lowerObjects.orbits.minBy({ _.representative.result.invariant });
      if canonicalReductionOrbit.contains(candidateUpperObject.inverse)
    ) yield candidateUpperObject.result
  }

  // and, for convenience, something to recursively find all children, filtering on a predicate
  def descendants(accept: A => Boolean = { _ => true }): Iterator[A] = {
    if (accept(this)) {
      Iterator(this) ++ children.flatMap(_.descendants(accept))
    } else {
      Iterator.empty
    }
  }
}

// An incomplete example, just to make sure the above framework is usable. 
case class TriangleFreeGraph(order: Int, edges: Set[Set[Int]]) extends CanonicalGeneration[TriangleFreeGraph, Seq[Int], TriangleFreeGraph] {
  override object automorphisms extends FinitelyGeneratedGroup[Seq[Int]] {
    override def generators = ??? // e.g. call nauty
  }
  override implicit val ordering: Ordering[TriangleFreeGraph] = {
    import net.tqft.toolkit.collections.LexicographicOrdering._
    Ordering.by(_.edges.toList.map(_.toList.sorted).sorted)
  }
  override def invariant = ??? // choose a canonical labelling, via nauty

  case class Upper(independentVertices: Set[Int]) {
    lazy val result = TriangleFreeGraph(order + 1, edges ++ independentVertices.map(Set(_, order + 1)))
    def inverse = result.Lower(order + 1)
  }
  case class Lower(k: Int) {
    lazy val result = TriangleFreeGraph(order - 1, edges.collect({ case e if !e.contains(k) => e.map({ case v if v > k => v - 1; case v => v }) }))
  }

  override def upperObjects = new automorphisms.Action[Upper] {
    override def elements = {
      // find independent sets
      def independentSubsetsOf(vertices: List[Int]): Iterator[List[Int]] = {
        vertices match {
          case Nil => Iterator(Nil)
          case head :: tail => {
            val connectedVertices = edges.filter(_.contains(head)).flatten
            independentSubsetsOf(tail) ++ independentSubsetsOf(tail.filterNot(connectedVertices.contains)).map(head :: _)
          }
        }
      }      
      (for(s <- independentSubsetsOf(0 until order toList)) yield Upper(s.toSet)).toSet
    }
    override def act(g: Seq[Int])(upper: Upper) = Upper(upper.independentVertices.map(g))
  }
  override def lowerObjects = new automorphisms.Action[Lower] {
    override def elements = (for (k <- 0 until order) yield Lower(k)).toSet
    override def act(g: Seq[Int])(lower: Lower) = Lower(g(lower.k))
  }
}