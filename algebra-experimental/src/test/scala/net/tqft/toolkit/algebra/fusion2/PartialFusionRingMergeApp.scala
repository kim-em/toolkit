package net.tqft.toolkit.algebra.fusion2

import net.tqft.toolkit.algebra.enumeration.TreeMerger
import java.io.File
import java.nio.file.Files
import scala.io.Source

object PartialFusionRingMergeApp extends App {

  val directory = new File("fusion-rings")

  TreeMerger.mergeDirectory(directory)

  import scala.collection.JavaConverters._
  import net.tqft.toolkit.collections.Iterators._

  val brokenFiles = Files.newDirectoryStream(directory.toPath, "*.tree").iterator.asScala.map(_.toFile).toSet.filter(f => Source.fromFile(f).getLines.last != " .")
  if (brokenFiles.nonEmpty) {
    val delete = args.length > 0 && args(0) == "-d"
    if (delete) {
      println("Warning: the following files were broken, and I'm deleting them now!")
    } else {
      println("Warning: the following files were broken: run with -d to delete them.")
    }
    for (f <- brokenFiles) {
      println(f)
      if (delete) f.delete
    }
  }

}