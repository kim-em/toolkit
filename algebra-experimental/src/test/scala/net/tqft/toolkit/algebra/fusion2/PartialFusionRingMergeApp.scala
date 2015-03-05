package net.tqft.toolkit.algebra.fusion2

import net.tqft.toolkit.algebra.enumeration.TreeMerger
import java.io.File

object PartialFusionRingMergeApp extends App {

  TreeMerger.mergeDirectory(new File("fusion-rings"))

}