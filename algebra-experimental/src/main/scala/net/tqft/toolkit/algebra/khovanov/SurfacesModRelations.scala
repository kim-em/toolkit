package net.tqft.toolkit.algebra.khovanov
import net.tqft.toolkit.algebra.AdditiveMonoid
import net.tqft.toolkit.algebra.Fraction

/**
 * @author scott
 */

object SurfacesModRelations
    extends AdditiveTensorCategory[GradedSmoothing, LinearComboCan]
    with Simplification[GradedSmoothing, LinearComboCan]
    with Factorisation[GradedSmoothing, LinearComboCan] {
  val mapMonoid = implicitly[AdditiveMonoid[Map[MonomialCan, Fraction[BigInt]]]]

  override def source(m: LinearComboCan) = m.source
  override def target(m: LinearComboCan) = m.target
  override def zero(x: GradedSmoothing, y: GradedSmoothing) = LinearComboCan(x, y, Map.empty)
  override def identity(x: GradedSmoothing) = MonomialCan(???, Map.empty)
  override def add(x: LinearComboCan, y: LinearComboCan) = {
    require(x.source == y.source)
    require(x.target == y.target)
    LinearComboCan(x.source, x.target, mapMonoid.add(x.terms, y.terms))
  }

  private def composeCans(x: Can, y: Can): LinearComboCan = {
    // recall we want this to work even when x.target ! = y.source; as long as one is contained in the other
    ???
  }
  private def composeMonomialCans(x: MonomialCan, y: MonomialCan) = ???

  override def compose(x: LinearComboCan, y: LinearComboCan) = {
    // just use distributivity
    ???
  }

  override def tensorObjects(x: GradedSmoothing, y: GradedSmoothing) = ???

  private def tensorCans(x: Can, y: Can): LinearComboCan = ???
  private def tensorMonomialCans(x: MonomialCan, y: MonomialCan) = ???

  override def tensorMorphisms(x: LinearComboCan, y: LinearComboCan) = {
    LinearComboCan(
      tensorObjects(x.source, y.source),
      tensorObjects(x.target, y.target),
      ???)
  }

  override def simplify(o: GradedSmoothing) = {
    // deloop
    ???
  }
  
  // only bother finding factorisations when a has a single term, and the underlying can is an identity
  override def findLeftFactorisationThrough(a: LinearComboCan)(b: LinearComboCan) = ???
  override def findRightFactorisationThrough(a: LinearComboCan)(b: LinearComboCan) = ???
}