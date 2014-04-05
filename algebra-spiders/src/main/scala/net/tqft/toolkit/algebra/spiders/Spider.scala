package net.tqft.toolkit.algebra.spiders

trait Spider[A] {
  def empty: A
  def rotate(a: A, k: Int): A
  def tensor(a1: A, a2: A): A
  def stitch(a: A): A

  def circumference(a: A): Int

  def stitchAt(a: A, k: Int): A = rotate(stitch(rotate(a, 1 - k)), k - 1)
  def stitchesAt(a: A, k: Int, m: Int): A = (k until k - m by -1).foldLeft(a)(stitchAt)

  def multiply(a1: A, a2: A, m: Int): A = {
    stitchesAt(tensor(a1, a2), circumference(a1), m)
  }

  def innerProduct(a1: A, a2: A): A = {
    multiply(a1, a2, circumference(a1))
  }
  def normSquared(a: A): A = {
    innerProduct(a, a)
  }
}

case class Disk[C](circumference: Int, contents: C)

object Spider {
  implicit def forget[A: DiagramSpider]: Spider[A] = implicitly[Spider[A]]

  implicit class DiskSpider[A](spider: Spider[A]) extends Spider[Disk[A]] {
    override def empty = Disk(0, spider.empty)
    override def rotate(disk: Disk[A], k: Int) = Disk(disk.circumference, spider.rotate(disk.contents, k))
    override def tensor(disk1: Disk[A], disk2: Disk[A]) = Disk(disk1.circumference + disk2.circumference, spider.tensor(disk1.contents, disk2.contents))
    override def stitch(disk: Disk[A]) = Disk(disk.circumference - 2, spider.stitch(disk.contents))
    override def circumference(disk: Disk[A]) = disk.circumference
  }

}
