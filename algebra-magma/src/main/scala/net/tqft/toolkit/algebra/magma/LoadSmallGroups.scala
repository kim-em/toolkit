package net.tqft.toolkit.algebra.magma

import java.io.File
import scala.io.Source
import net.tqft.toolkit.algebra.grouptheory.FinitelyGeneratedFiniteGroup
import net.tqft.toolkit.permutations.Permutations.Permutation
import net.tqft.toolkit.algebra.grouptheory.FiniteGroups

object LoadSmallGroups {
  def apply(): Map[Int, IndexedSeq[(FinitelyGeneratedFiniteGroup[Permutation], Seq[FinitelyGeneratedFiniteGroup[Permutation]#ActionOnFiniteSet[Int]])]] = {
    import net.tqft.toolkit.collections.Split._
    val groupsText = Source.fromURL(this.getClass.getResource("/small-groups.txt")).getLines.toSeq.splitOn(_ == "---").map(_.dropWhile(_.isEmpty))
    val groups = for(groupText <- groupsText) yield {
      val SmallGroup = """SmallGroup\((\d+), (\d+)\)""".r
      import net.tqft.toolkit.Extractors._
      val SmallGroup(Int(order), Int(index)) = groupText.head
      val generatorsText +: actionsText = groupText.tail.splitOn(_.isEmpty).toSeq.filter(_.nonEmpty)
      val generators = generatorsText.map(_.split(",").toIndexedSeq.map(_.toInt))
      val actionsData = actionsText.map(_.map(_.split(",").toIndexedSeq.map(_.toInt)))
      val group = FiniteGroups.symmetricGroup(generators.head.size).subgroupGeneratedBy(generators)
      val actions = actionsData.map(actionData => new group.ActionOnFiniteSet[Int] { 
        override def elements = 0 until actionData.head.size
        override def act(g: Permutation, x: Int) = {
          group.elementsAsWordsInGenerators(g).foldRight(x)({ case (i, y) => actionData(i)(y) })
        }
      })
      (group, actions)
    }
    groups.groupBy(_._1.size).mapValues(_.toIndexedSeq)
  }
}