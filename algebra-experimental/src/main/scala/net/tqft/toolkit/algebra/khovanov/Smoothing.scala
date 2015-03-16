package net.tqft.toolkit.algebra.khovanov

import scala.language.implicitConversions
import net.tqft.toolkit.algebra.Fraction
import net.tqft.toolkit.algebra.AdditiveSemigroup
import net.tqft.toolkit.algebra.AdditiveMonoid
import scala.collection.mutable.Buffer
import net.tqft.toolkit.algebra.Field

case class Strand(arcs: Seq[Int])

case class Smoothing(strands: Seq[Strand])

case class GradedSmoothing(q: Int, smoothing: Smoothing)

case class Circle(arcs: Seq[Int])

case class Can(source: GradedSmoothing, target: GradedSmoothing, dots: Map[Circle, Boolean])

object Can {
  def identity(s: GradedSmoothing): Can = ???
}

case class MonomialCan(can: Can, beta: Int, gamma: Int)

case class LinearComboCan(source: GradedSmoothing, target: GradedSmoothing, terms: Map[MonomialCan, Fraction[BigInt]])

object LinearComboCan {
  implicit def liftToLinearCombo(can: MonomialCan) = LinearComboCan(can.can.source, can.can.target, Map(can -> 1))
}

object SurfacesModRelations extends TensorCategory[GradedSmoothing, LinearComboCan] {
  val mapMonoid = implicitly[AdditiveMonoid[Map[MonomialCan, Fraction[BigInt]]]]

  override def source(m: LinearComboCan) = m.source
  override def target(m: LinearComboCan) = m.target
  override def zero(x: GradedSmoothing, y: GradedSmoothing) = LinearComboCan(x, y, Map.empty)
  override def identity(x: GradedSmoothing) = MonomialCan(Can.identity(x), 0, 0)
  override def add(x: LinearComboCan, y: LinearComboCan) = {
    require(x.source == y.source)
    require(x.target == y.target)
    LinearComboCan(x.source, x.target, mapMonoid.add(x.terms, y.terms))
  }

  private def composeCans(x: Can, y: Can): LinearComboCan = ???
  private def composeMonomialCans(x: MonomialCan, y: MonomialCan) = ???

  override def compose(x: LinearComboCan, y: LinearComboCan) = {
    require(x.target == y.source)
    LinearComboCan(x.source, y.target, ???)
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
}

trait Category[O, M] {
  def source(x: M): O
  def target(x: M): O
  def identity(o: O): M
  def compose(x: M, y: M): M
}
trait CategoryWithZero[O, M] extends Category[O, M] {
  def zero(source: O, target: O): M
}
trait AdditiveCategory[O, M] extends CategoryWithZero[O, M] with AdditiveSemigroup[M]
trait TensorCategory[O, M] extends AdditiveCategory[O, M] {
  def tensorObjects(x: O, y: O): O
  def tensorMorphisms(x: M, y: M): M
}

trait Matrix[O, M] {
  def source: Seq[O]
  def target: Seq[O]
  def nonzeroEntries: TraversableOnce[(Int, Int, M)]
  def deleteRow(row: Int): Matrix[O, M]
  def deleteColumn(column: Int): Matrix[O, M]
  def takeRow(row: Int): Matrix[O, M]
  def takeColumn(column: Int): Matrix[O, M]
}

case class SparseMatrix[O, M](override val source: Seq[O], override val target: Seq[O], override val nonzeroEntries: Seq[(Int, Int, M)]) extends Matrix[O, M] {
  override def deleteRow(row: Int) = SparseMatrix(
    source,
    target.take(row) ++ target.drop(row + 1),
    nonzeroEntries.collect({
      case (r, c, x) if r < row => (r, c, x)
      case (r, c, x) if r > row => (r - 1, c, x)
    }))
  override def deleteColumn(column: Int) = SparseMatrix(
    source.take(column) ++ source.drop(column + 1),
    target,
    nonzeroEntries.collect({
      case (r, c, x) if c < column => (r, c, x)
      case (r, c, x) if c > column => (r, c - 1, x)
    }))
  override def takeRow(row: Int) = SparseMatrix(
    source,
    Seq(target(row)),
    nonzeroEntries.collect({
      case (r, c, x) if r == row => (0, c, x)
    }))
  override def takeColumn(column: Int) = SparseMatrix(
    Seq(source(column)),
    target,
    nonzeroEntries.collect({
      case (r, c, x) if c == column => (r, 0, x)
    }))
}

object Matrix {
  implicit def matricesOverCategory[O, M](implicit c: AdditiveCategory[O, M]): AdditiveCategory[Seq[O], Matrix[O, M]] = new MatricesOverCategory[O, M](c)
  class MatricesOverCategory[O, M](c: AdditiveCategory[O, M]) extends AdditiveCategory[Seq[O], Matrix[O, M]] {
    override def source(m: Matrix[O, M]) = m.source
    override def target(m: Matrix[O, M]) = m.target
    override def identity(x: Seq[O]) = SparseMatrix(x, x, x.zipWithIndex.map(p => (p._2, p._2, c.identity(p._1))))
    override def zero(source: Seq[O], target: Seq[O]) = SparseMatrix(source, target, Stream.empty)
    override def add(x: Matrix[O, M], y: Matrix[O, M]) = {
      require(x.source == y.source)
      require(x.target == y.target)
      val newEntries = (x.nonzeroEntries.toSeq ++ y.nonzeroEntries).groupBy(t => (t._1, t._2)).mapValues(v => c.sum(v.map(_._3))).toSeq.map(p => (p._1._1, p._1._2, p._2))
      SparseMatrix(x.source, x.target, newEntries)
    }
    override def compose(x: Matrix[O, M], y: Matrix[O, M]) = ???
  }
  implicit def matricesOverTensorCategory[O, M](implicit c: TensorCategory[O, M]): TensorCategory[Seq[O], Matrix[O, M]] = new MatricesOverTensorCategory[O, M](c)
  class MatricesOverTensorCategory[O, M](c: TensorCategory[O, M]) extends MatricesOverCategory[O, M](c) with TensorCategory[Seq[O], Matrix[O, M]] {
    override def tensorObjects(x: Seq[O], y: Seq[O]) = ???
    override def tensorMorphisms(x: Matrix[O, M], y: Matrix[O, M]) = ???
  }
}

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

  implicit def complexesOverTensorCategory[O, M](implicit c: TensorCategory[O, M]) = new ComplexesOverTensorCategory(c)
  implicit class ComplexesOverTensorCategory[O, M](c: TensorCategory[O, M]) extends ComplexesOverAdditiveCategory[O, M](c) with TensorCategory[Complex[O, M], ChainMap[O, M]] {
    override def tensorObjects(x: Complex[O, M], y: Complex[O, M]) = ???
    override def tensorMorphisms(x: ChainMap[O, M], y: ChainMap[O, M]) = ???
  }
}

trait GaussianEliminator[O, M] {
  def locatePseudoIsomorphism(m: Matrix[O, M]): Option[(Int, Int)]
  def pseudoInverse(m: M): M
  def invertible_?(m: M): Boolean

  def oneStep(c: Complex[O, M]): Option[(Int, Complex[O, M], Option[Complex[O, M]])] = ???
  def decompose(c: Complex[O, M]): Seq[Complex[O, M]] = {
    def recursivelyDecompose(summands: Seq[Complex[O, M]], height: Int, remaining: Complex[O, M]): Seq[Complex[O, M]] = {
      oneStep(remaining) match {
        case None => remaining +: summands
        case Some((newHeight, stillRemaining, optionalNewSummand)) => recursivelyDecompose(summands ++ optionalNewSummand, newHeight, stillRemaining)
      }
    }
    recursivelyDecompose(Seq.empty, c.homologicalHeight, c)
  }
}

object SurfacesGaussianEliminator extends GaussianEliminator[GradedSmoothing, LinearComboCan] {
  override def locatePseudoIsomorphism(m: Matrix[GradedSmoothing, LinearComboCan]) = ???
  override def pseudoInverse(can: LinearComboCan) = {
    require(can.terms.size == 1)
    require(can.source == can.target)
    val mCan = can.terms.head._1
    val rationals = implicitly[Field[Fraction[BigInt]]]
    LinearComboCan(can.source, can.source, Map(MonomialCan(mCan.can, -mCan.beta, -mCan.gamma) -> rationals.inverse(can.terms.head._2)))
  }
  override def invertible_?(can: LinearComboCan) = {
    val mCan = can.terms.head._1
    mCan.beta == 0 && mCan.gamma == 0
  }
}

case class Crossing(sign: Int, strands: (Int, Int, Int, Int))
case class AnnularTangle(cut: Seq[Int], crossings: Seq[Crossing])
case class AnnularCobordism()

trait Functor[O1, M1, O2, M2] {
  def applyToObject(o: O1): O2
  def applyToMorphism(m: M1): M2
}

object Types {
  type O1 = GradedSmoothing
  type M1 = LinearComboCan
  type O2 = Seq[O1]
  type M2 = Matrix[O1, M1]
  type O3 = Complex[O2, M2]
  type M3 = ChainMap[O2, M2]
  type O4 = Seq[O3]
  type M4 = Matrix[O3, M3]
}

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
  
  override def applyToObject(t: AnnularTangle) = t.crossings.map(applyToCrossing).reduce(combineComplexes)
  override def applyToMorphism(m: AnnularCobordism) = ???
}

object ImplicitVerification {
  import Types._
  implicit def S = SurfacesModRelations
  implicitly[TensorCategory[O1, M1]]
  implicitly[TensorCategory[O2, M2]]
  implicitly[TensorCategory[O3, M3]]
}