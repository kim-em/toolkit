package net.tqft.toolkit.algebra.fusion

import net.tqft.toolkit.algebra.enumeration.TreeReader
import java.io.File

object PartialFusionRingVerifyApp2 extends App {

  val directory = new File("fusion-rings2")
  TreeReader.verify2(directory, args.filterNot(_.startsWith("-")).mkString(" "), args.contains("-d"))

}