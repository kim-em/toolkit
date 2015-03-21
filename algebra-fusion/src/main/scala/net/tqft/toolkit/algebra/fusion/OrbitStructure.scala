package net.tqft.toolkit.algebra.fusion

import net.tqft.toolkit.algebra.magma.LoadSmallGroups
import net.tqft.toolkit.algebra.grouptheory.FinitelyGeneratedFiniteGroup
import net.tqft.toolkit.algebra.enumeration.Odometer

object OrbitStructure {
  def apply(shortString: String) = unapply(shortString).get
  def unapply(shortString: String): Option[OrbitStructure] = {
    import net.tqft.toolkit.Extractors._
    try {
      shortString.split(",").toSeq match {
        case Int(groupOrder) +: Int(groupIndex) +: pairs => {
          val AnObjectRegex = """AnObject(\d+)""".r
          val actionObjectPairs = pairs.sliding(2, 2).toSeq.map({
            case Seq(Int(action), AnObjectRegex(Int(n))) => (action, AnObject(n))
            case Seq(Int(action), "Dimension2Object") => (action, Dimension2Object)
            case Seq(Int(action), "Generic2SupertransitiveFusionObject") => (action, Generic2SupertransitiveFusionObject)
            case Seq(Int(action), "GenericDepth2FusionObject") => (action, GenericDepth2FusionObject)
            case Seq(Int(action), "GenericFusionObject") => (action, GenericFusionObject)
          })
          Some(OrbitStructure(groupOrder, groupIndex, actionObjectPairs))
        }
      }
    } catch {
      case e: Exception => None
    }
  }
}

case class OrbitStructure(groupOrder: Int, groupIndex: Int, actionObjectPairs: Seq[(Int, SmallFusionObject)]) {
  def toShortString = {
    groupOrder + "," + groupIndex + "," + actionObjectPairs.map(p => p._1 + "," + p._2).mkString(",")
  }

  lazy val (group, actions) = OrbitStructures.groupsDatabase(groupOrder)(groupIndex - 1)
  lazy val elements = {
    import Ordering.Implicits._
    group.elements.toSeq.sorted
  }
  lazy val objectTypes = (TrivialObject +:
    Seq.fill(groupOrder - 1)(NonTrivialObject)) ++
    actionObjectPairs.zip(orbitSizes).flatMap({
      case ((_, objectType), multiplicity) => Seq.fill(multiplicity)(objectType)
    })

  def compatibleDualData: Iterator[IndexedSeq[Int]] = {
    val groupInvolution = IndexedSeq.tabulate(groupOrder)({ i => (0 until groupOrder).find(j => groupMultiplication(i, j) == 0).get })

    val dualitiesByObjectType = (for (objectType <- actionObjectPairs.map(_._2).distinct) yield {
      objectType -> {
        val numberOfOrbits = actionObjectPairs.count(_._2 == objectType)
        val orbitSizesOfThisType = actionObjectPairs.zip(orbitSizes).collect({
          case ((_, obj), size) if obj == objectType => size
        })
        val zeroes = List.tabulate(numberOfOrbits)(i => List.fill(i + 1)(0))
        def completeToSymmetricMatrix(lowerTriangularMatrix: Seq[Seq[Int]]) = {
          List.tabulate(lowerTriangularMatrix.size, lowerTriangularMatrix.size)({
            case (i, j) if j > i => lowerTriangularMatrix(j)(i)
            case (i, j) => lowerTriangularMatrix(i)(j)
          })
        }
        def diagonal(m: Seq[Seq[Int]]) = m.zipWithIndex.map(p => p._1(p._2))
        def limit(lowerTriangularMatrix: List[List[Int]]): Boolean = {
          completeToSymmetricMatrix(lowerTriangularMatrix).map(_.sum).zip(orbitSizesOfThisType).forall(p => p._1 <= p._2)
        }
        Odometer(limit)(zeroes).filter(m => diagonal(m).forall(_ % 2 == 0)).map(completeToSymmetricMatrix).toStream
      }
    }).toMap
    val dualities = dualitiesByObjectType.foldLeft(Stream(Map[SmallFusionObject, Seq[Seq[Int]]]()))({
      case (bigStream, (objectType, stream)) => bigStream.flatMap(m => stream.map(d => m + (objectType -> d)))
    })
    def buildInvolution(m: Map[SmallFusionObject, Seq[Seq[Int]]]): IndexedSeq[Int] = {
      ???
    }
    dualities.map(buildInvolution).iterator
  }

  def groupMultiplication(i: Int, j: Int): Int = elements.indexOf(group.multiply(elements(i), elements(j)))

  def orbitSizes = for ((cosetActionIndex, _) <- actionObjectPairs) yield {
    OrbitStructures.groupsDatabase(groupOrder)(groupIndex - 1)._2(cosetActionIndex).elements.size
  }

  override def toString = {
    s"OrbitStructure(groupOrder = $groupOrder, groupIndex = $groupIndex, " +
      actionObjectPairs.zip(orbitSizes).map({ case ((cosetActionIndex, objectType), orbitSize) => s"($cosetActionIndex, /* $orbitSize, */ $objectType)" }) +
      ")"
  }

}

trait DecompositionKnowledge
case object YouKnowNothing extends DecompositionKnowledge
case class Exactly(map: Map[SmallFusionObject, Int]) extends DecompositionKnowledge
case class AtLeast(map: Map[SmallFusionObject, Int]) extends DecompositionKnowledge // AtLeast must specify the exact multiplicities of any object types that appear, but other object types may also appear in XXdual.

sealed trait SmallFusionObject {
  def dimension: Double
  def XXdual: DecompositionKnowledge
}
sealed trait SmallFusionObjectWithDefiniteDimension extends SmallFusionObject
sealed trait InvertibleObject extends SmallFusionObjectWithDefiniteDimension {
  override def dimension = 1.0
  override def XXdual = Exactly(Map(TrivialObject -> 1))
}
case object TrivialObject extends InvertibleObject
case object NonTrivialObject extends InvertibleObject // these are allowed to be involutions!
case object InvolutionObject extends InvertibleObject // these are required to be nontrivial!

case class AnObject(n: Int) extends SmallFusionObjectWithDefiniteDimension {
  override def dimension = 2 * math.cos(math.Pi / (n + 1))
  override def XXdual = {
    n match {
      case 3 => {
        Exactly(Map(TrivialObject -> 1, InvolutionObject -> 1))
      }
      case 4 => {
        Exactly(Map(TrivialObject -> 1, AnObject(4) -> 1))
      }
      case 5 => {
        Exactly(Map(TrivialObject -> 1, Dimension2Object -> 1))
      }
      case _ => YouKnowNothing
    }
  }
}
case object Dimension2Object extends SmallFusionObjectWithDefiniteDimension {
  override def dimension = 2.0
  override def XXdual = YouKnowNothing // TODO actually, we know quite a lot, but it's too messy for just now.
}
case object Generic2SupertransitiveFusionObject extends SmallFusionObject {
  override def dimension = math.sqrt(21.0 / 4.0)
  override def XXdual = Exactly(Map(TrivialObject -> 1, GenericDepth2FusionObject -> 1))
}
case object GenericDepth2FusionObject extends SmallFusionObject {
  override def dimension = 4.25
  override def XXdual = YouKnowNothing // actually, if this object is called Y, we have Y=Y^*, and Y^2 = 1 + m Y + ... with m >= 1.
}
case object GenericFusionObject extends SmallFusionObject {
  override def dimension = math.sqrt(6.0)
  override def XXdual = YouKnowNothing
}

object OrbitStructures {
  val groupsDatabase = LoadSmallGroups()

  def apply(globalDimension: Double): Iterator[OrbitStructure] = {
    // FIXME decide which object types are allowed
    apply(globalDimension, Seq(AnObject(3), AnObject(4), Generic2SupertransitiveFusionObject, GenericDepth2FusionObject, GenericFusionObject))
  }

  def apply(globalDimension: Double, allowedObjects: Seq[SmallFusionObject]): Iterator[OrbitStructure] = {
    for (groupOrder <- (1 to globalDimension.toInt).iterator; groupIndex <- 1 to groupsDatabase(groupOrder).size; os <- apply(groupOrder, groupIndex, globalDimension, allowedObjects)) yield os
  }

  def apply(groupOrder: Int, groupIndex: Int, globalDimension: Double, allowedObjects: Seq[SmallFusionObject]): Iterator[OrbitStructure] = {
    val maximumNumberOfOrbits = (globalDimension / groupOrder).toInt
    val (group, actions) = groupsDatabase(groupOrder)(groupIndex - 1)

    def actionObjectPairCompatible_?(action: FinitelyGeneratedFiniteGroup[net.tqft.toolkit.permutations.Permutations.Permutation]#ActionOnFiniteSet[Int], obj: SmallFusionObject): Boolean = {
      obj match {
        case obj: SmallFusionObjectWithDefiniteDimension => {
          // See the lemma below
          group.size <= action.elements.size * obj.dimension * obj.dimension
        }
        case _ => true
      }
    }

    val actionObjectPairs = for (
      (action, actionIndex) <- actions.zipWithIndex;
      obj <- allowedObjects;
      if actionObjectPairCompatible_?(action, obj)
    ) yield {
      // This needs to be a lemma somewhere: if the orbit is G/H, and X = eH, then XX^* contains H, so dim(x) >= sqrt(#H).
      (actionIndex, obj, math.max(action.elements.size * obj.dimension * obj.dimension, group.size.toDouble))
    }

    def limit(r: List[Int]) = {
      r.zip(actionObjectPairs).map({ case (multiplicity, (_, _, orbitDimension)) => multiplicity * orbitDimension }).sum <= globalDimension - group.size
    }

    val possibilities = Odometer(limit _)(List.fill(actionObjectPairs.size)(0))

    def orbitStructureConsistent_?(orbits: Seq[(Int, SmallFusionObject)]): Boolean = {
      def orbitsContainsObjects_?(objects: Set[SmallFusionObject]) = {
        val nonInvertibleObjects = objects.filterNot(_.isInstanceOf[InvertibleObject])

        nonInvertibleObjects.forall(o => orbits.map(_._2).contains(o)) &&
          (!objects.contains(NonTrivialObject) || groupOrder > 1) &&
          (!objects.contains(InvolutionObject) || groupOrder % 2 == 0)
      }

      val objectTypes = orbits.map(_._2).distinct

      (!objectTypes.contains(GenericDepth2FusionObject) || objectTypes.contains(Generic2SupertransitiveFusionObject)) &&
        objectTypes.forall({
          case obj =>
            obj.XXdual match {
              case YouKnowNothing => true
              case AtLeast(map) => {
                orbitsContainsObjects_?(map.keySet)
              }
              case Exactly(map) => {
                orbitsContainsObjects_?(map.keySet)
              }
            }
        })
    }

    for (
      multiplicities <- possibilities;
      pairs = multiplicities.zip(actionObjectPairs);
      sequence = pairs.flatMap({ case (m, (index, obj, _)) => Seq.fill(m)(index, obj) });
      if (orbitStructureConsistent_?(sequence))
    ) yield {
      OrbitStructure(groupOrder, groupIndex, sequence)
    }
  }
}