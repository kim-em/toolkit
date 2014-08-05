package net.tqft.toolkit.algebra.fusion

import net.tqft.toolkit.amazon.S3

object ClearPartialFusionRingBucket extends App {
  S3.withAccount("AKIAIUHOCNVIDRMVRC2Q")("partial-fusion-ring").clear
}