package net.tqft.toolkit.algebra.spiders

import net.tqft.toolkit.algebra.spiders.PlanarGraph
import net.tqft.toolkit.algebra.spiders.DrawPlanarGraph

object scratch {
  val K = PlanarGraph(3, Vector(List(), List((1, 4), (7, 3), (2, 2), (6, 1)),
List((7, 2), (15, 3), (8, 6), (14, 5)), List((4, 6), (9, 8), (3, 7),
(8, 5)), List((14, 2), (3, 5), (13, 7), (2, 1)), List((6, 4), (13,
1), (5, 7), (12, 9)), List((10, 6), (5, 9), (9, 7), (4, 8)),
List((11, 10), (1, 3), (12, 4), (16, 9)), List((15, 6), (11, 3), (16, 10), (10, 9))), Vector((2,0), (2,0), (2,0), (2,0), (2,0), (2,0),
(2,0), (2,0)), 0)                                 //> K  : net.tqft.toolkit.algebra.spiders.PlanarGraph = PlanarGraph(3,Vector(Lis
                                                  //| t(), List((1,4), (7,3), (2,2), (6,1)), List((7,2), (15,3), (8,6), (14,5)), L
                                                  //| ist((4,6), (9,8), (3,7), (8,5)), List((14,2), (3,5), (13,7), (2,1)), List((6
                                                  //| ,4), (13,1), (5,7), (12,9)), List((10,6), (5,9), (9,7), (4,8)), List((11,10)
                                                  //| , (1,3), (12,4), (16,9)), List((15,6), (11,3), (16,10), (10,9))),Vector((2,0
                                                  //| ), (2,0), (2,0), (2,0), (2,0), (2,0), (2,0), (2,0)),0)
    
  DrawPlanarGraph.createPDF(K)                    //> crossings: Map(5 -> 0, 1 -> 0, 6 -> 0, 2 -> 0, 7 -> 0, 3 -> 0, 8 -> 0, 4 -> 
                                                  //| 0)
                                                  //| Jul 25, 2016 1:17:43 AM com.github.fommil.jni.JniLoader liberalLoad
                                                  //| INFO: successfully loaded /tmp/jniloader211065360701790740netlib-native_syst
                                                  //| em-linux-x86_64.so
                                                  //| Jul 25, 2016 1:17:44 AM com.github.fommil.jni.JniLoader load
                                                  //| INFO: already loaded netlib-native_system-linux-x86_64.so
                                                  //| res0: java.nio.file.Path = /tmp/planar-graphs5815803136175058406/urn:sha1:63
                                                  //| e81dd9be3a2f80840e4a707a41048fe16b1137.pdf
}