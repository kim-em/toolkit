package net.tqft.toolkit.algebra

import net.tqft.toolkit.PackageImplicits

package object principalgraphs extends PackageImplicits {
  implicit class RichString(s: String) {
    def split_!(delimiter: String): Seq[String] = {
      if (s == "") {
        Seq.empty
      } else {
        if (s.endsWith(delimiter)) {
          split_!(s.stripSuffix(delimiter)) :+ ""
        } else {
          s.split(delimiter).toSeq
        }
      }
    }
  }
}