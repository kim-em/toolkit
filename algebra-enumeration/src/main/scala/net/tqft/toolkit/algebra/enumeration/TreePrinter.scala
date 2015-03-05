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

case class TreePrinter[A](stringify: A => String, level: A => Int, accept: A => Double = { a: A => 1 }, out: PrintWriter = new PrintWriter(System.out)) {
  def to(pw: Writer): TreePrinter[A] = this.copy(out = new PrintWriter(pw))
  def to(ps: PrintStream): TreePrinter[A] = this.copy(out = new PrintWriter(ps))
  def to(filename: String): TreePrinter[A] = to(new File(filename))
  def to(file: File): TreePrinter[A] = to(new PrintStream(new FileOutputStream(file)))
  def to(directory: String, a: A): TreePrinter[A] = to(new File(directory), a)
  def to(directory: File, a: A): TreePrinter[A] = to(directory.toPath.resolve(stringify(a) + ".tree").toFile)

  def print(iterator: Iterator[A]) {
    val stack = scala.collection.mutable.Stack[A]()

    var offset = -1

    def spaces: Stream[String] = "" #:: spaces.map(_ + " ")

    def printDots {
      val y = stack.pop
      if (accept(y) > 0) {
        out.println(spaces(level(y) + 1 - offset) + ".")
      }
    }

    for (x <- iterator) {
      if (offset == -1) offset = level(x)
      while (stack.nonEmpty && level(stack.top) >= level(x)) printDots
      stack.push(x)
      out.println(spaces(level(x) - offset) + stringify(x))
    }

    while (stack.nonEmpty) printDots

    out.flush
  }
}

object TreeReader {
  def readLeaves(file: File): Iterator[String] = {
    if (file.isDirectory) {
      import scala.collection.JavaConverters._
      for (file <- Files.newDirectoryStream(file.toPath, "*.tree").iterator.asScala.map(_.toFile); leaf <- readLeaves(file)) yield leaf
    } else {
      readLeaves(Source.fromFile(file).getLines)
    }
  }
  def readLeaves(lines: Iterator[String]): Iterator[String] = {
    def indenting(s: String) = s.indexWhere { _ != ' ' }
    (lines ++ Iterator("")).sliding(2).collect({ case Seq(s1, s2) if s1.trim != "." && indenting(s1) >= indenting(s2) => s1.trim })
  }
}

object TreeMerger {
  def mergeDirectory(directory: File = new File(System.getProperty("user.dir")), filenamer: String => String = { s => s }) {
    import scala.collection.JavaConverters._
    mergeFiles(Files.newDirectoryStream(directory.toPath, "*.tree").iterator.asScala.map(_.toFile), filenamer, directory)
  }

  def mergeFiles(files: TraversableOnce[File], filenamer: String => String = { s => s }, outputDir: File = new File(System.getProperty("user.dir"))): Set[File] = {
    val fileStream = files.toStream
    println("Merging " + fileStream.mkString(" + "))
    val merges = for (
      file1 <- fileStream;
      file2 <- fileStream;
      if file1 != file2;
      tmp = File.createTempFile("tree-merger-tmp", ".tree");
      merged = merge(file1, file2, new PrintWriter(new FileOutputStream(tmp)))
    ) yield (file1, file2, merged, tmp)

    merges.flatMap({
      case (_, _, false, tmp) => {
        tmp.delete
        None
      }
      case (file1, file2, true, tmp) => {
        file1.delete
        file2.delete
        val newPath = outputDir.toPath.resolve(filenamer(Source.fromFile(tmp).getLines.next) + ".tree")
        Files.move(tmp.toPath, newPath, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
        Some((file1, file2, newPath.toFile))
      }
    }).toStream.headOption match {
      case Some((deletedFile1, deletedFile2, newFile)) => {
        mergeFiles(fileStream.toSet - deletedFile1 - deletedFile2 + newFile, filenamer, outputDir)
      }
      case None => fileStream.toSet
    }

  }

  def merge(tree1: File, tree2: File, out: Writer): Boolean = {
    merge(Source.fromFile(tree1).getLines, Source.fromFile(tree2).getLines, out)
  }
  def merge(tree1: String, tree2: String, out: Writer): Boolean = {
    merge(Source.fromString(tree1).getLines, Source.fromString(tree2).getLines, out)
  }

  case class StringTree(root: String, children: Option[Iterator[StringTree]])

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
    pw.flush
    merged
  }

}