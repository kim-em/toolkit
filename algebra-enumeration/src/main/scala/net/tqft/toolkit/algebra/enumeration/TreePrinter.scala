package net.tqft.toolkit.algebra.enumeration

import java.io.PrintWriter
import java.io.PrintStream
import java.io.FileOutputStream
import java.io.File
import scala.io.Source
import java.io.Writer
import java.nio.file.Paths
import java.nio.file.Path
import java.nio.file.Files
import java.io.Reader
import java.io.FileInputStream
import java.io.BufferedReader
import java.io.FileReader
import java.io.FileWriter
import net.tqft.toolkit.Logging
import net.tqft.toolkit.collections.Iterators._

case class TreePrinter[A](stringify: A => String, level: A => Int, accept: A => Double = { a: A => 1 }, out: PrintWriter = new PrintWriter(System.out)) {
  def to(pw: Writer): TreePrinter[A] = this.copy(out = new PrintWriter(pw))
  def to(ps: PrintStream): TreePrinter[A] = this.copy(out = new PrintWriter(ps))
  def to(filename: String): TreePrinter[A] = to(new File(filename))
  def to(file: File): TreePrinter[A] = to(new PrintStream(new FileOutputStream(file)))
  def to(directory: String, a: A): TreePrinter[A] = to(new File(directory), a)
  def to(directory: String, filename: String): TreePrinter[A] = to(new File(directory), filename)
  def to(directory: File, a: A): TreePrinter[A] = to(directory, stringify(a))
  def to(directory: File, filename: String): TreePrinter[A] = to(directory.toPath.resolve(filename + ".tree").toFile)

  def print(iterator: Iterator[A]) {
    val stack = scala.collection.mutable.Stack[(A, Double)]()

    var offset = -1

    def spaces: Stream[String] = "" #:: spaces.map(_ + " ")

    def printDots {
      val (y, a) = stack.pop
      if (a > 0) {
        out.println(spaces(level(y) + 1 - offset) + ".")
      }
    }

    for (x <- iterator) {
      if (offset == -1) offset = level(x)
      while (stack.nonEmpty && level(stack.top._1) >= level(x)) printDots
      stack.push((x, accept(x)))
      out.println(spaces(level(x) - offset) + stringify(x))
    }

    while (stack.nonEmpty) printDots

    out.flush
  }
}

object TreeHelper {
  def firstLine(file: File): String = {
    val reader = new BufferedReader(new FileReader(file))
    val line = reader.readLine
    reader.close
    line
  }

  def firstLineIfMoreThanOneLine(file: File): Option[String] = {
    val reader = new BufferedReader(new FileReader(file))
    val line = reader.readLine
    val result = if (reader.readLine == null) {
      None
    } else {
      Some(line)
    }
    reader.close
    result
  }

  def lines(f: File): Iterator[String] = {
    new Iterator[String] {
      val br = new BufferedReader(new FileReader(f))
      var cached: Option[String] = None
      var closed = false
      def readCache = {
        br.readLine match {
          case null => {
            closed = true
            br.close
            cached = None
            false
          }
          case line => {
            cached = Some(line)
            true
          }
        }
      }
      override def hasNext = {
        !closed && (cached.nonEmpty || readCache)
      }
      override def next = {
        if (!hasNext) Iterator.empty.next
        val r = cached.get
        cached = None
        r
      }
    }
  }

}

object TreeReader {

  def readLeaves(file: File, prefix: String = ""): Iterator[String] = {
    if (file.isDirectory) {
      import scala.collection.JavaConverters._

      def files = Files.newDirectoryStream(file.toPath, prefix + "*.tree")
        .iterator
        .asScala
        .map(_.toFile)

      val roots = (for (file <- files; root <- TreeHelper.firstLineIfMoreThanOneLine(file)) yield root).toSet

      for (
        file <- files;
        leaf <- readLeaves(file);
        if !roots.contains(leaf)
      ) yield leaf
    } else {
      readLeaves(TreeHelper.lines(file))
    }
  }
  private def indenting(s: String) = s.indexWhere { _ != ' ' }
  def readLeaves(lines: Iterator[String]): Iterator[String] = {
    (lines ++ Iterator("")).sliding(2).collect({ case Seq(s1, s2) if s1.trim != "." && indenting(s1) >= indenting(s2) => s1.trim })
  }

  def verify(file: File, prefix: String = "", delete: Boolean = false) {
    if (file.isDirectory) {
      import scala.collection.JavaConverters._

      def files = Files.newDirectoryStream(file.toPath, prefix + "*.tree")
        .iterator
        .asScala
        .map(_.toFile)

      for (file <- files) verify(file, prefix, delete && file.getName.split(" ")(2).filterNot(_ == '_').nonEmpty)
    } else {
      val lines = TreeHelper.lines(file)
      def parse(line: String) = {
        import net.tqft.toolkit.Extractors._
        val Seq(objects, Int(level), matrices, _) = line.trim.split(" ").toSeq
        val Seq(Int(selfDual), Int(dualPairs)) = objects.split(",").toSeq
        val rank = selfDual + 2 * dualPairs
        val matrixEntries = if (matrices.contains(",") || rank == 2) {
          matrices.split(",")
        } else {
          require(level < 10)
          matrices.toCharArray().map(_.toString)
        }
        (line, level, matrixEntries.zipWithIndex.collect({ case ("_", i) => i }).toSet)
      }

      try {
        var stack: List[(String, Int, Set[Int])] = parse(lines.next) :: Nil
        var s = 1

        for ((line, lineNo) <- lines.zipWithIndex) {
          val i = indenting(line)
          if (i != s && i != s - 1) {
            println(s"Indenting problem in $file:")
            println(stack.head._1)
            println(line)
            if (delete) {
              println("Deleting!")
              file.delete
            }
          }
          if (line.trim == ".") {
            stack = stack.tail
            s = s - 1
          } else {
            if (i == s - 1) {
              stack = stack.tail
              s = s - 1
            }
            val p = parse(line)
            if (p._2 > stack.head._2 || p._3.forall(i => stack.head._3.contains(i))) {
              // looks good
              stack = p :: stack
              s = s + 1
            } else {
              println(s"Invalid child in $file:")
              println(stack.head)
              println(p)
              if (delete) {
                println("Deleting!")
                file.delete
              }
            }
          }
        }
      } catch {
        case e: Exception => {
          println(s"Invalid syntax in $file:")
          println(e)
          if (delete) {
            println("Deleting!")
            file.delete
          }
        }
      }
    }
  }
  def verify2(file: File, prefix: String = "", delete: Boolean = false) {
    if (file.isDirectory) {
      import scala.collection.JavaConverters._

      def files = Files.newDirectoryStream(file.toPath, prefix + "*.tree")
        .iterator
        .asScala
        .map(_.toFile)

      for (file <- files) verify2(file, prefix, delete && file.getName.split(" ")(4).filterNot(_ == '_').nonEmpty)
    } else {
      val lines = TreeHelper.lines(file)
      def parse(line: String) = {
        import net.tqft.toolkit.Extractors._
        val Seq(Int(rank), _, _, Int(level), matrices, _) = line.trim.split(" ").toSeq
        val matrixEntries = if (matrices.contains(",") || rank == 2) {
          matrices.split(",")
        } else {
          require(level < 10)
          matrices.toCharArray().map(_.toString)
        }
        (line, level, matrixEntries.zipWithIndex.collect({ case ("_", i) => i }).toSet)
      }

      try {
        var stack: List[(String, Int, Set[Int])] = parse(lines.next) :: Nil
        var s = 1

        for ((line, lineNo) <- lines.zipWithIndex) {
          val i = indenting(line)
          if (i != s && i != s - 1) {
            println(s"Indenting problem in $file:")
            println(stack.head._1)
            println(line)
            if (delete) {
              println("Deleting!")
              file.delete
            }
          }
          if (line.trim == ".") {
            stack = stack.tail
            s = s - 1
          } else {
            if (i == s - 1) {
              stack = stack.tail
              s = s - 1
            }
            val p = parse(line)
            if (p._2 > stack.head._2 || p._3.forall(i => stack.head._3.contains(i))) {
              // looks good
              stack = p :: stack
              s = s + 1
            } else {
              println(s"Invalid child in $file:")
              println(stack.head)
              println(p)
              if (delete) {
                println("Deleting!")
                file.delete
              }
            }
          }
        }
      } catch {
        case e: Exception => {
          println(s"Invalid syntax in $file:")
          println(e)
          if (delete) {
            println("Deleting!")
            file.delete
          }
        }
      }
    }
  }
}

object TreeMerger {
  var pleaseFinishNow = false

  def fileComplete(file: File): Boolean = {
    val reader = new BufferedReader(new FileReader(file))
    var lastLine: String = null
    var currentLine: String = null
    while ({ currentLine = reader.readLine; currentLine != null }) {
      lastLine = currentLine
    }
    reader.close
    lastLine == " ."
  }

  def mergeDirectory(directory: File = new File(System.getProperty("user.dir")), filenamer: String => String = { s => s }) {
    import scala.collection.JavaConverters._
    import net.tqft.toolkit.collections.Iterators._
    val files = Files.newDirectoryStream(directory.toPath, "*.tree").iterator.asScala.map(_.toFile).toSeq.par.filter(fileComplete).seq.sortBy(f => -f.length)

    mergeFiles(files, filenamer, directory)
  }

  def mergeFiles(toMerge: Seq[File], filenamer: String => String = { s => s }, outputDir: File = new File(System.getProperty("user.dir"))): Seq[File] = {
    println(s"Merging ${toMerge.size} files.")
    val firstLines = toMerge.par.map(f => (TreeHelper.firstLine(f), f)).seq.toMap
    val newFiles = (for (
      mergeTo <- toMerge;
      if mergeTo.exists;
      if !pleaseFinishNow
    ) yield {
      println("Merging into " + mergeTo)

      val tmp = File.createTempFile("tree-merger-tmp", ".tree")
      val pw = new PrintWriter(new FileWriter(tmp))

      val mergedFiles = scala.collection.mutable.Set[File]()

      for (line <- mergeManyFiles(TreeHelper.lines(mergeTo), firstLines.filter(_._2 != mergeTo), mergedFiles)) {
        pw.println(line)
      }

      pw.close

      if (mergedFiles.isEmpty) {
        tmp.delete
        None
      } else {
        println("Finished merging")
        mergeTo.delete
        for (f <- mergedFiles) f.delete

        val newPath = outputDir.toPath.resolve(filenamer(TreeHelper.firstLine(tmp)) + ".tree")
        Files.move(tmp.toPath, newPath)

        Some(newPath.toFile)
      }
    }).toSet.flatten

    if (newFiles.nonEmpty && !pleaseFinishNow) {
      mergeFiles((toMerge ++ newFiles).distinct.filter(_.exists), filenamer, outputDir)
    } else {
      toMerge
    }
  }

  def mergeManyFiles(mergeTo: Iterator[String], mergeFrom: Map[String, File], mergedFiles: scala.collection.mutable.Set[File]): Iterator[String] = {
    import PeekableIterator._
    import FancyIterator._
    new Iterator[String] {
      var iterator: FancyIterator[String] = mergeTo.fancy

      val offset = indenting(iterator.peek.get)

      override def hasNext: Boolean = {
        iterator.hasNext
      }
      override def next: String = {
        iterator.peek match {
          case Some(next) => {
            mergeFrom.get(next.trim) match {
              case Some(file) if file.exists => {
                println("      <--- " + file)
                mergedFiles += file
                val fileIterator = TreeHelper.lines(file).peekable
                val shift = indenting(next) - indenting(fileIterator.peek.get)
                val padding = if (shift > 0) Seq.fill(shift)(" ").mkString else ""
                def shiftString(s: String) = {
                  if (shift <= 0) {
                    s.drop(-shift)
                  } else {
                    padding + s
                  }
                }
                iterator = mergeIterators(iterator.simplify, fileIterator.map(shiftString))
                iterator.next
              }
              case _ => iterator.next
            }
          }
          case None => Iterator.empty.next
        }
      }
    }
  }



  trait Simplifiable[X] { self: X =>
    def simplify: X
  }

  trait FancyIterator[X] extends PeekableIterator[X] with Simplifiable[FancyIterator[X]]

  object FancyIterator {
    implicit class Fancyable[A](iterator: Iterator[A]) {
      def fancy: FancyIterator[A] = new FancyIterator[A] {
        private val peekableIterator = {
          import PeekableIterator._
          iterator.peekable
        }
        override def simplify = this
        override def hasNext = peekableIterator.hasNext
        override def next = peekableIterator.next
        override def peek = peekableIterator.peek
      }
    }
  }

  private def mergeIterators(iterator1: FancyIterator[String], iterator2: Iterator[String]): FancyIterator[String] = {
    val peekable1 = iterator1
    val peekable2 = PeekableIterator(iterator2)
    new FancyIterator[String] {
      override def simplify = {
        if (!peekable1.hasNext) {
          ???
        } else if (!peekable2.hasNext) {
          peekable1.simplify
        } else {
          this
        }
      }
      override def hasNext = peekable1.hasNext || peekable2.hasNext
      override def peek = {
        if (peekable1.hasNext) {
          if (peekable2.hasNext) {
            val p1 = peekable1.peek
            val p2 = peekable2.peek
            val i1 = indenting(p1.get)
            val i2 = indenting(p2.get)
            if (i1 == i2) {
              if (p1 != p2) {
                Logging.warn("Lines did not agree:")
                Logging.warn(p1.get)
                Logging.warn(p2.get)
                ???
              }
              p1
            } else if (i1 < i2) {
              p2
            } else {
              p1
            }
          } else {
            peekable1.peek
          }
        } else {
          peekable2.peek
        }
      }
      override def next = {
        if (peekable1.hasNext) {
          if (peekable2.hasNext) {
            val i1 = indenting(peekable1.peek.get)
            val i2 = indenting(peekable2.peek.get)
            if (i1 == i2) {
              val n1 = peekable1.next
              val n2 = peekable2.next
              if (n1 != n2) {
                Logging.warn("Lines did not agree:")
                Logging.warn(n1)
                Logging.warn(n2)
                ???
              }
              n1
            } else if (i1 < i2) {
              peekable2.next
            } else {
              peekable1.next
            }
          } else {
            peekable1.next
          }
        } else {
          peekable2.next
        }
      }
    }
  }

  private def indenting(s: String) = s.indexWhere { _ != ' ' }


}