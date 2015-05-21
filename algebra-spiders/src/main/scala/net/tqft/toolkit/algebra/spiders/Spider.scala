package net.tqft.toolkit.algebra.spiders

trait Spider[A] {
  def empty: A
  def strand: A
  def rotate(a: A, k: Int): A
  def tensor(a1: A, a2: A): A
  def tensorProduct(as: Seq[A]): A = as.reduce(tensor)
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
  def assembleAlongPlanarPartition(partition: Seq[Seq[Int]], ds: Seq[A]): A = {
    // partition is a planar partition of 0, ..., n-1
    // Validity checks
    require(partition.head.head == 0, "Begin partition from 0")
    require(isContiguous(partition.flatten.sorted), "Not a planar partition")
    require(isSorted(for (s <- partition) yield s.head), "Give partition in sequential order")
    require(partition.length == ds.length, "Partition does not match objects")
    for (k <- 0 until partition.length) {
      require(isSorted(partition(k)), s"Partition cell $k needs to be sorted")
      require(partition(k).length == circumference(ds(k)), s"Object in index $k does not have the correct circumference")
    }

    def isSorted(S: Seq[Int]): Boolean = (S == S.sorted)
    def isContiguous(S: Seq[Int]): Boolean = (S == (S.head to S.last).toSeq)
    def mod(a: Int, b: Int): Int = ((a % b) + b) % b

    def assemble(partition: Seq[Seq[Int]], ds: Seq[A]): A = {
      if (partition.length == 1) {
        require(isContiguous(partition.head), "Invalid partition")
        return ds.head
      } else if (isContiguous(partition.head)) {
        val newPartition = partition.tail.map(_.map((x: Int) => x - partition.head.length))
        return tensor(assemble(newPartition, ds.tail), ds.head)
      } else {
        val n = partition.flatten.length
        val zipped = partition zip ds
        val newPartitionAndObjects = zipped.map((X: Tuple2[Seq[Int], A]) =>
          (X._1.map((x: Int) => mod(x - 1, n)).sorted, if (partition.head == X._1) rotate(X._2, 1) else X._2)).
          sortWith((X1: Tuple2[Seq[Int], A], X2: Tuple2[Seq[Int], A]) => X1._1.head < X2._1.head).
          unzip
        return rotate(assemble(newPartitionAndObjects._1, newPartitionAndObjects._2), -1)
      }
    }

    assemble(partition, ds)
  }
}

case class Disk[C](circumference: Int, contents: C)

object Spider {
  implicit def forget[A: DiagramSpider]: Spider[A] = implicitly[Spider[A]]

  implicit class DiskSpider[A](spider: Spider[A]) extends Spider[Disk[A]] {
    override def empty = Disk(0, spider.empty)
    override def strand = Disk(2, spider.strand)
    override def rotate(disk: Disk[A], k: Int) = Disk(disk.circumference, spider.rotate(disk.contents, k))
    override def tensor(disk1: Disk[A], disk2: Disk[A]) = Disk(disk1.circumference + disk2.circumference, spider.tensor(disk1.contents, disk2.contents))
    override def stitch(disk: Disk[A]) = Disk(disk.circumference - 2, spider.stitch(disk.contents))
    override def circumference(disk: Disk[A]) = disk.circumference
  }

}
