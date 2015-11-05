package net.tqft.toolkit.algebra.spiders

object scratch extends Plantri {
  val (bytes, log) = runWithByteOutput("5 -P3 -Edhoc2m2", true)
                                                  //> Running plantri:
                                                  //| /home/jayce/bin/plantri45/plantri 5 -P3 -Edhoc2m2 -v 
                                                  //| 1 isomorphism classes
                                                  //| 1 duals of disk triangulations written to stdout; cpu=0.00 sec
                                                  //| bytes  : Array[Byte] = Array(23, 0, 1, 2, -1, 3, 0, 4, -1, 5, 3, 6, -1, 1, 5
                                                  //| , 7, -1, 8, 6, 4, -1, 7, 8, 2)
                                                  //| log  : List[String] = List("/home/jayce/bin/plantri45/plantri 5 -P3 -Edhoc2m
                                                  //| 2 -v ", 1 isomorphism classes, 1 duals of disk triangulations written to std
                                                  //| out; cpu=0.00 sec)
  parseEdgeCodeBytes(bytes)                       //> res0: Seq[IndexedSeq[IndexedSeq[Int]]] = List(Vector(Vector(0, 1, 2), Vector
                                                  //| (3, 0, 4), Vector(5, 3, 6), Vector(1, 5, 7), Vector(8, 6, 4), Vector(7, 8, 2
                                                  //| )))
  parseEdgeCodeBytes(bytes) map edgeAdjListToPlanarGraph map DrawPlanarGraph.createPDF
                                                  //> crossings: Map()
                                                  //| Jul 14, 2015 7:51:43 PM com.github.fommil.jni.JniLoader liberalLoad
                                                  //| INFO: successfully loaded /tmp/jniloader7347477186222782885netlib-native_sys
                                                  //| tem-linux-x86_64.so
                                                  //| Jul 14, 2015 7:51:43 PM com.github.fommil.jni.JniLoader load
                                                  //| INFO: already loaded netlib-native_system-linux-x86_64.so
                                                  //| res1: Seq[java.nio.file.Path] = List(/tmp/planar-graphs7749546399521690927/u
                                                  //| rn:sha1:92ba25df32dce44710f93ab809830cdceb98926c.pdf)
}