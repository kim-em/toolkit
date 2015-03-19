package net.tqft.toolkit.algebra.fusion

import net.tqft.toolkit.algebra.magma.LoadSmallGroups
import net.tqft.toolkit.algebra.grouptheory.FinitelyGeneratedFiniteGroup
import net.tqft.toolkit.algebra.enumeration.Odometer

case class OrbitStructure(groupOrder: Int, groupIndex: Int, actionObjectPairs: Seq[(Int, SmallFusionObject)]) {
  //  lazy val (group, actions) = OrbitStructures.groupsDatabase(groupOrder)(groupIndex - 1)

}

sealed trait SmallFusionObject {
  def dimension: Double
}
sealed trait SmallFusionObjectWithDefiniteDimension extends SmallFusionObject
case class AnObject(n: Int) extends SmallFusionObjectWithDefiniteDimension {
  override def dimension = 2*math.cos(math.Pi/(n+1))
}
case object Dimension2Object extends SmallFusionObjectWithDefiniteDimension {
  override def dimension = 2.0
}
case object GenericFusionObject extends SmallFusionObject {
  override def dimension = math.sqrt(21.0/4.0)
}

object OrbitStructures {
  val groupsDatabase = LoadSmallGroups()

  println(groupsDatabase.keySet)
  
  def apply(globalDimension: Double): Iterator[OrbitStructure] = {
    // FIXME decide which object types are allowed
    apply(globalDimension, Seq(AnObject(3), GenericFusionObject))
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
          group.size <= action.elements.size * obj.dimension * obj.dimension
        }
        case _ => true
      }
    }
    
    val actionObjectPairs = for (
      (action, actionIndex) <- actions.zipWithIndex;
      obj <- allowedObjects;
      if actionObjectPairCompatible_?(action,obj)
    ) yield {
      (actionIndex, obj, math.max(action.elements.size * obj.dimension * obj.dimension, group.size.toDouble))
    }
    
    def limit(r: List[Int]) = {
       r.zip(actionObjectPairs).map({ case (multiplicity, (_, _, orbitDimension)) => multiplicity * orbitDimension }).sum <= globalDimension - group.size
    }
    
    Odometer(limit _)(List.fill(actionObjectPairs.size)(0)).map(multiplicities => OrbitStructure(groupOrder, groupIndex, multiplicities.zip(actionObjectPairs).flatMap({case (m, (index, obj, _)) => Seq.fill(m)(index, obj) })))
  }
}