package net.tqft.toolkit.algebra.khovanov

object AnnularKhovanovHomology extends Functor[AnnularTangle, AnnularCobordism, Types.O3, Types.M3] {
  val category = {
    implicit def S = SurfacesModRelations
    implicitly[TensorCategory[Types.O3, Types.M3]]
  }

  private def applyToCrossing(c: Crossing): Types.O3 = ???
  private def combineComplexes(x: Types.O3, y: Types.O3): Types.O3 = {
    contract(deloop(category.tensorObjects(x, y)))
  }
  
  def deloop(x: Types.O3): Types.O3 = ???
  def contract(x: Types.O3): Types.O3 = ???
  
  override def applyToObject(t: AnnularTangle) = t.tangle.crossings.map(applyToCrossing).reduce(combineComplexes)
  override def applyToMorphism(m: AnnularCobordism) = ???
}

