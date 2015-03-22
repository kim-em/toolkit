package net.tqft.toolkit.algebra.fusion

import net.tqft.toolkit.algebra.enumeration.TreeMerger
import java.io.File
import java.nio.file.Files
import scala.io.Source
import scala.concurrent.Future
import scala.io.StdIn

object PartialFusionRingMergeApp extends App {

  val directory1 = new File("fusion-rings")
  val directory2 = new File("fusion-rings2")

  import scala.concurrent.ExecutionContext.Implicits.global
  Future {
    println("Hit <enter> to request that everyone finishes up quickly and goes home.")
    StdIn.readLine
    println("Cleaning up ...")
    TreeMerger.pleaseFinishNow = true
  }

  TreeMerger.mergeDirectory(directory1)
  TreeMerger.mergeDirectory(directory2)

  import scala.collection.JavaConverters._
  import net.tqft.toolkit.collections.Iterators._

  val brokenFiles = Files.newDirectoryStream(directory1.toPath, "*.tree").iterator.asScala.map(_.toFile).toSet.par.filter(f => !TreeMerger.fileComplete(f)) ++
    Files.newDirectoryStream(directory2.toPath, "*.tree").iterator.asScala.map(_.toFile).toSet.par.filter(f => !TreeMerger.fileComplete(f))
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