package net.tqft.toolkit.arithmetic

import scala.language.implicitConversions

object Mod {
  implicit class ModdableInt(x: Int) {
    def mod(k: Int) = {
      if (k == 0) {
        x
      } else {
        if (x < 0) {
          -((-x - 1) % k) + k - 1
        } else {
          x % k
        }
      }
    }
  }
  implicit class ModdableLong(x: Long) {
    def mod(k: Long) = {
      if (k == 0) {
        x
      } else {
        if (x < 0) {
          -((-x - 1) % k) + k - 1
        } else {
          x % k
        }
      }
    }
  }
}