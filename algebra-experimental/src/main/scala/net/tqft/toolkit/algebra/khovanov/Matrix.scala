package net.tqft.toolkit.algebra.khovanov

trait Matrix[O, M] {
  def source: Seq[O]
  def target: Seq[O]
  def nonzeroEntries: TraversableOnce[(Int, Int, M)]
  def deleteRow(row: Int): Matrix[O, M]
  def deleteColumn(column: Int): Matrix[O, M]
  def takeRow(row: Int): Matrix[O, M]
  def takeColumn(column: Int): Matrix[O, M]
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
  implicit def matricesOverTensorCategory[O, M](implicit c: AdditiveTensorCategory[O, M]): AdditiveTensorCategory[Seq[O], Matrix[O, M]] = new MatricesOverTensorCategory[O, M](c)
  class MatricesOverTensorCategory[O, M](c: AdditiveTensorCategory[O, M]) extends MatricesOverCategory[O, M](c) with AdditiveTensorCategory[Seq[O], Matrix[O, M]] {
    override def tensorObjects(x: Seq[O], y: Seq[O]) = ???
    override def tensorMorphisms(x: Matrix[O, M], y: Matrix[O, M]) = ???
  }
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

