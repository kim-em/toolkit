package net.tqft.toolkit.algebra.grouptheory

import net.tqft.toolkit.algebra._
import net.tqft.toolkit.algebra.matrices._
import net.tqft.toolkit.algebra.categories._
import scala.reflect.ClassTag

trait Representation[A, F] extends Homomorphism[Group, A, Matrix[F]] { representation =>
  def degree: Int
  override val source: FiniteGroup[A]
  override def target = ??? // GL(F, n)?

  def character(implicit addition: AdditiveMonoid[F]): Seq[F] = {
    for (c <- source.conjugacyClasses; g = c.representative) yield {
      apply(g).trace
    }
  }

  def irrepMultiplicities(implicit evidence: F =:= Fraction[Int]): Seq[Int] = {
    val chi = character(Rationals.asInstanceOf[Field[F]]).map(evidence)
    for (c <- source.reducedCharacters) yield {
      source.characterPairing(chi, c)
    }
  }
  def basisForIsotypicComponent(chi: source.RationalCharacter)(implicit field: Field[F], manifest: ClassTag[F]): Seq[Seq[F]] = {
    require(chi.character.size == source.conjugacyClasses.size)
    val matrices = Matrices.matricesOver(degree)(field)
    var j = 0
    def blip {
      j = j + 1
      if (j % 1000 == 0) println(j / 1000)
    }
    def volatileZeroMatrix = {
      import net.tqft.toolkit.collections.SparseSeq
      matrices.zero.par.mapRows({ r => r.toArray[F] })
    }
    def projectorConjugacyClass(cc: Set[A], c: Fraction[Int]) = {
      val result = matrices.scalarMultiply(field.fromRational(c), cc.foldLeft(volatileZeroMatrix)({ (m: Matrix[F], g: A) => blip; matrices.add(m, representation(g)) }))
      require(result.entries.head.isInstanceOf[scala.collection.mutable.WrappedArray[_]])
      result
    }
    val projector = (source.conjugacyClasses zip chi.character).foldLeft(volatileZeroMatrix)({
      (m: Matrix[F], p: (Orbit[A, A], Fraction[Int])) =>
        {
          Representation.info("Preparing projector to the " + chi.character + " isotypic component in conjugacy class " + p._1.representative)
          matrices.add(m, projectorConjugacyClass(p._1.elements, p._2))
        }
    }).ensuring(_.entries.head.isInstanceOf[scala.collection.mutable.WrappedArray[_]]).mapRows(_.toList.toIndexedSeq)
    //    val chidegree = (chi.character.head.ensuring(_.denominator == 1)).numerator
    //    val eigenvalue = field.quotientByInt(field.fromInt(source.size), chidegree)
    //    projector.eigenspace(eigenvalue)
    projector.par.rowEchelonForm.entries.filter({ x => x.find(_ != field.zero).nonEmpty }).seq
  }
}

object Representation extends net.tqft.toolkit.Logging