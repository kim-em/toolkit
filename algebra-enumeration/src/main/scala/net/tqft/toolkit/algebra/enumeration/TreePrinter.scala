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

case class TreePrinter[A](stringify: A => String, level: A => Int, accept: A => Double = { a: A => 1 }, out: PrintWriter = new PrintWriter(System.out)) {
  def to(pw: Writer): TreePrinter[A] = this.copy(out = new PrintWriter(pw))
  def to(ps: PrintStream): TreePrinter[A] = this.copy(out = new PrintWriter(ps))
  def to(filename: String): TreePrinter[A] = to(new File(filename))
  def to(file: File): TreePrinter[A] = to(new PrintStream(new FileOutputStream(file)))
  def to(directory: String, a: A): TreePrinter[A] = to(new File(directory), a)
  def to(directory: File, a: A): TreePrinter[A] = to(directory.toPath.resolve(stringify(a) + ".tree").toFile)

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
        if(!hasNext) Iterator.empty.next
        val r = cached.get
        cached = None
        r
      }
    }
  }

}

object TreeReader {

  def readLeaves(file: File): Iterator[String] = {
    if (file.isDirectory) {
      import scala.collection.JavaConverters._

      def files = Files.newDirectoryStream(file.toPath, "*.tree")
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
  def readLeaves(lines: Iterator[String]): Iterator[String] = {
    def indenting(s: String) = s.indexWhere { _ != ' ' }
    (lines ++ Iterator("")).sliding(2).collect({ case Seq(s1, s2) if s1.trim != "." && indenting(s1) >= indenting(s2) => s1.trim })
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
    val files = Files.newDirectoryStream(directory.toPath, "*.tree").iterator.asScala.map(_.toFile).toSet.filter(fileComplete)

    mergeFiles(files, filenamer, directory)
  }

  def mergeFiles(toMerge: Set[File], filenamer: String => String = { s => s }, outputDir: File = new File(System.getProperty("user.dir"))): Set[File] = {
    println(s"Merging ${toMerge.size} files.")
    val firstLines = toMerge.map(f => (TreeHelper.firstLine(f), f)).toMap
    def findMatchingLineInFile(f: File): Option[File] = {
      val reader = new BufferedReader(new FileReader(f))
      var found: Option[File] = None
      var currentLine: String = null
      while (found.isEmpty && { currentLine = reader.readLine; currentLine != null }) {
        found = firstLines.get(currentLine.trim)
        if (found.nonEmpty && found.get == f) found = None
      }
      reader.close
      found
    }
    def findMatchingLinesInFile(f: File): Iterator[File] = {
      TreeHelper.lines(f).flatMap(line => firstLines.get(line.trim)).filter(_ != f)
    }
    val pairs = toMerge.par.flatMap({ f => findMatchingLinesInFile(f).map(g => (f, g)) }).seq
    //    val pairsWithOverlaps = toMerge.par.map({ f => (f, findMatchingLineInFile(f))}).collect({ case (f, Some(g)) => (f,g) }).seq
    //    println(s"Found ${pairsWithOverlaps.size} pairs (with overlaps) of files to merge.")
    //    val (mergingFiles, pairs) = pairsWithOverlaps.foldLeft((Set.empty[File], List.empty[(File, File)]))({ case ((seen, pairsSoFar), (f,g)) => if(seen.contains(f) || seen.contains(g)) (seen, pairsSoFar) else (seen + f + g, (f,g) :: pairsSoFar)})
    println(s"Found ${pairs.size} pairs of files to merge.")
    val newFiles = for ((f, g) <- pairs; if f.exists && g.exists) yield {
      val tmp = File.createTempFile("tree-merger-tmp", ".tree")
      println(s"merging $f <--- $g")
      require(merge(f, g, new PrintWriter(new FileOutputStream(tmp))))
      //      println(s"deleting $f")
      f.delete
      //      println(s"deleting $g")
      g.delete
      val newPath = outputDir.toPath.resolve(filenamer(TreeHelper.firstLine(tmp)) + ".tree")
      //      println(s"writing $newPath")
      Files.move(tmp.toPath, newPath)
      newPath.toFile
    }
    if (newFiles.nonEmpty && !pleaseFinishNow) {
      mergeFiles((toMerge ++ newFiles).filter(_.exists), filenamer, outputDir)
    } else {
      toMerge
    }
  }

  def merge(tree1: File, tree2: File, out: Writer): Boolean = {
    merge(TreeHelper.lines(tree1), TreeHelper.lines(tree2), out)
  }
  def merge(tree1: String, tree2: String, out: Writer): Boolean = {
    merge(Source.fromString(tree1).getLines, Source.fromString(tree2).getLines, out)
  }

  trait PeekableIterator[A] extends Iterator[A] {
    def peek: Option[A]
  }

  object PeekableIterator {
    def apply[A](iterator: Iterator[A]): PeekableIterator[A] = PeekableIteratorImplementation(iterator)
    private case class PeekableIteratorImplementation[A](iterator: Iterator[A], var nextOption: Option[A] = None) extends PeekableIterator[A] {
      override def hasNext = nextOption.nonEmpty || iterator.hasNext
      override def next = {
        nextOption match {
          case Some(a) => {
            nextOption = None
            a
          }
          case None => iterator.next
        }
      }
      override def peek: Option[A] = {
        nextOption match {
          case Some(a) => Some(a)
          case None => {
            if (iterator.hasNext) {
              nextOption = Some(iterator.next)
              nextOption
            } else {
              None
            }
          }
        }
      }
      override def map[B](f: A => B) = PeekableIteratorImplementation(iterator.map(f), nextOption.map(f))
    }

  }

  private def mergeIterators(iterator1: Iterator[String], iterator2: Iterator[String]): Iterator[String] = {
    val peekable1 = PeekableIterator(iterator1)
    val peekable2 = PeekableIterator(iterator2)
    new Iterator[String] {
      override def hasNext = peekable1.hasNext || peekable1.hasNext
      override def next = {
        if (peekable1.hasNext) {
          if (peekable2.hasNext) {
            val i1 = indenting(peekable1.peek.get)
            val i2 = indenting(peekable2.peek.get)
            if (i1 == i2) {
              peekable1.next.ensuring(_ == peekable2.next)
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

  def merge(tree1: Iterator[String], tree2: Iterator[String], out: Writer): Boolean = {
    val pw = new PrintWriter(out)
    val (head, offset) = {
      val untrimmedHead = tree2.next
      (untrimmedHead.trim, indenting(untrimmedHead))
    }
    var merged = false
    for (x <- tree1) {
      if (x.trim == head) {
        merged = true
        val shift = indenting(x) - offset
        val padding = if (shift > 0) Seq.fill(shift)(" ").mkString else ""
        pw.println(x)
        def shiftString(s: String) = {
          if (shift <= 0) {
            s.drop(-shift)
          } else {
            padding + s
          }
        }

        for (y <- mergeIterators(tree1, tree2.map(shiftString))) {
          pw.println(y)
        }
      } else {
        pw.println(x)
      }
    }
    // make sure we exhaust the iterator
    for (x <- tree2) {}
    pw.close
    merged
  }

}