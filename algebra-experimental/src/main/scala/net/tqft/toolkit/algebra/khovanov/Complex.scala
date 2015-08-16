
package net.tqft.toolkit.algebra.khovanov

case class Complex[O, M](homologicalHeight: Int, objects: Seq[O], differentials: Seq[M])

case class ChainMap[O, M](homologicalHeight: Int, source: Complex[O, M], target: Complex[O, M], components: Seq[M])

object ChainMap {
  implicit class ComplexesOverCategory[O, M](c: Category[O, M]) extends Category[Complex[O, M], ChainMap[O, M]] {
    override def source(x: ChainMap[O, M]) = x.source
    override def target(x: ChainMap[O, M]) = x.target
    override def identity(x: Complex[O, M]) = ChainMap(x.homologicalHeight, x, x, x.objects.map(c.identity))
    override def compose(x: ChainMap[O, M], y: ChainMap[O, M]) = ???
  }
  implicit class ComplexesOverAdditiveCategory[O, M](c: AdditiveCategory[O, M]) extends ComplexesOverCategory[O, M](c) with AdditiveCategory[Complex[O, M], ChainMap[O, M]] {
    override def zero(x: Complex[O, M], y: Complex[O, M]) = ???
    override def add(x: ChainMap[O, M], y: ChainMap[O, M]) = ???
  }

  implicit def complexesOverTensorCategory[O, M](implicit c: AdditiveTensorCategory[O, M]): AdditiveTensorCategory[Complex[O, M], ChainMap[O, M]] = new ComplexesOverTensorCategory(c)
  implicit class ComplexesOverTensorCategory[O, M](c: AdditiveTensorCategory[O, M]) extends ComplexesOverAdditiveCategory[O, M](c) with AdditiveTensorCategory[Complex[O, M], ChainMap[O, M]] {
    override def tensorObjects(x: Complex[O, M], y: Complex[O, M]) = ???
    override def tensorMorphisms(x: ChainMap[O, M], y: ChainMap[O, M]) = ???
  }
}

