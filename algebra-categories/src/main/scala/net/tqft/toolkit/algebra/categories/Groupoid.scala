package net.tqft.toolkit.algebra.categories

import net.tqft.toolkit.algebra._

trait Groupoid[O, M] extends Category[O, M] with WithInverses[M]