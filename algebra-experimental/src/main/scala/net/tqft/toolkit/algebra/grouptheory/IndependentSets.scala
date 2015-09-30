package net.tqft.toolkit.algebra.grouptheory

import net.tqft.toolkit.permutations.Permutations._
import net.tqft.toolkit.algebra.enumeration.CanonicalGeneration

/**
 * @author scott
 */
case class IndependentSets(n: Int) {

  val S = FiniteGroups.symmetricGroup(n)

  case class IndependentSet(elements: Set[Permutation]) extends CanonicalGeneration[IndependentSet, Permutation] { is =>

    lazy val subgroup = S.subgroupGeneratedBy(elements.toSeq)
    
    val automorphisms = {
      // this is really stupid; work out how to use nauty
      S.subgroupGeneratedBy(S.elements.toSeq.filter(p => elements.map(q => S.multiply(p, q, S.inverse(p))) == elements))
    }

    case class Lower(toDelete: Permutation) {
      def result = IndependentSet(elements - toDelete)
    }

    override lazy val lowerObjects = new automorphisms.ActionOnFiniteSet[Lower] {
      override val elements = is.elements.map(Lower)
      override def act(g: IndexedSeq[Int], lower: Lower) = {
        Lower(S.multiply(g, lower.toDelete, S.inverse(g)))
      }
    }
    
    override val ordering = new Ordering[lowerObjects.Orbit] {
      override def compare(x: lowerObjects.Orbit, y: lowerObjects.Orbit) = {
        // this is a disaster; implement properly
        import Ordering.Implicits._
        implicitly[Ordering[Permutation]].compare(x.elements.map(_.toDelete).min, y.elements.map(_.toDelete).min)
      }
    }
    
    case class Upper(toAdd: Permutation) {
      lazy val result = IndependentSet(elements + toAdd)
      def inverse = result.Lower(toAdd)
    }

    override lazy val upperObjects = new automorphisms.ActionOnFiniteSet[Upper] {
      override val elements = {
        (S.elements -- subgroup.elements).filter({g => 
          is.elements.forall({h => 
            !S.subgroupGeneratedBy((is.elements + g - h).toSeq).elements.contains(h)
          })  
        }).map(Upper)
      }
      override def act(g: IndexedSeq[Int], upper: Upper) = {
        Upper(S.multiply(g, upper.toAdd, S.inverse(g)))
      }
    }
  }

}