package net.tqft.toolkit.algebra.fusion2

import net.tqft.toolkit.algebra.enumeration.TreeReader
import java.io.File

object PartialFusionRingVerifyApp extends App {

  val directory = new File("fusion-rings")

  TreeReader.verify(directory)


}