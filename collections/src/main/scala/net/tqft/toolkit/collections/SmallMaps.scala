package net.tqft.toolkit.collections

import scala.collection.generic.CanBuildFrom

object SmallMaps {
  def empty[A, B] = EmptyMap.asInstanceOf[Map[A, B]]

  object EmptyMap extends Map[Any, Nothing] {
    override def +[B](kv: (Any, B)) = updated(kv._1, kv._2)
    override def updated[B1 >: Nothing](key: Any, value: B1): Map[Any, B1] = new Map1(key, value)
    override def -(k: Any) = Map.empty.-(k)
    override def get(k: Any) = None
    override def iterator = Iterator.empty
    override def keys = Iterable.empty
    override def keySet = SmallSets.empty
    override def keysIterator = Iterator.empty
    override def filter(p: ((Any, Nothing)) => Boolean) = this
    override def filterKeys(p: Any => Boolean) = this
    override def map[B, That](f: ((Any, Nothing)) ⇒ B)(implicit bf: CanBuildFrom[Map[Any, Nothing], B, That]): That = bf().result()
    override def mapValues[B](f: Nothing => B) = SmallMaps.empty[Any, B]
    override def foreach[U](f: ((Any, Nothing)) => U) {}
    override def contains(key: Any) = false
    override def isEmpty = true
    override def nonEmpty = false
  }

  protected trait SmallNonEmptyMap[A, +B] { this: Map[A, B] =>
    override def isEmpty = false
    override def nonEmpty = true
    override def keys = keySet
    override def keysIterator = keySet.iterator
  }

  class Map1[A, +B](key1: A, value1: B) extends Map[A, B] with SmallNonEmptyMap[A, B] {
    override def +[C >: B](kv: (A, C)) = {
      if (key1 == kv._1) {
        if (value1 == kv._2) this
        else new Map1(key1, kv._2)
      } else {
        new Map2(key1, value1, kv._1, kv._2)
      }
    }
    override def -(k: A) = {
      if (k == key1) {
        SmallMaps.empty
      } else {
        this
      }
    }
    override def get(k: A) = {
      if (k == key1) {
        Some(value1)
      } else {
        None
      }
    }
    override def iterator = Iterator((key1, value1))
    override def keySet = new SmallSets.Set1(key1)
    override def filter(p: ((A, B)) => Boolean) = {
      if (p((key1, value1))) this
      else empty
    }
    override def filterKeys(p: A => Boolean) = {
      if (p(key1)) {
        this
      } else {
        SmallMaps.empty
      }
    }
    override def mapValues[C](f: B => C) = new Map1(key1, f(value1))
    override def contains(key: A) = key == key1
    override def foreach[U](f: ((A, B)) => U) {
      f((key1, value1))
    }
  }
  class Map2[A, +B](key1: A, value1: B, key2: A, value2: B) extends Map[A, B] with SmallNonEmptyMap[A, B] {
    override def +[C >: B](kv: (A, C)) = {
      if (key1 == kv._1) {
        if (value1 == kv._2) this
        else new Map2(key1, kv._2, key2, value2)
      } else if (key2 == kv._1) {
        if (value2 == kv._2) this
        else new Map2(key1, value1, key2, kv._2)
      } else {
        new Map3(key1, value1, key2, value2, kv._1, kv._2)
      }
    }
    override def -(k: A) = {
      if (k == key1) {
        new Map1(key2, value2)
      } else if (k == key2) {
        new Map1(key1, value1)
      } else {
        this
      }
    }
    override def get(k: A) = {
      if (k == key1) {
        Some(value1)
      } else if (k == key2) {
        Some(value2)
      } else {
        None
      }
    }
    override def iterator = Iterator((key1, value1), (key2, value2))
    override def keySet = new SmallSets.Set2(key1, key2)
    override def filter(p: ((A, B)) => Boolean) = {
      (p((key1, value1)), p((key2, value2))) match {
        case (true, true) => this
        case (true, false) => new Map1(key1, value1)
        case (false, true) => new Map1(key2, value2)
        case (false, false) => SmallMaps.empty
      }
    }
    override def filterKeys(p: A => Boolean) = {
      (p(key1), p(key2)) match {
        case (true, true) => this
        case (true, false) => new Map1(key1, value1)
        case (false, true) => new Map1(key2, value2)
        case (false, false) => SmallMaps.empty
      }
    }
    override def mapValues[C](f: B => C) = new Map2(key1, f(value1), key2, f(value2))
    override def contains(key: A) = key == key1 || key == key2
    override def foreach[U](f: ((A, B)) => U) {
      f((key1, value1))
      f((key2, value2))
    }
  }
  class Map3[A, +B](key1: A, value1: B, key2: A, value2: B, key3: A, value3: B) extends Map[A, B] with SmallNonEmptyMap[A, B] {
    override def +[C >: B](kv: (A, C)) = {
      if (key1 == kv._1) {
        if (value1 == kv._2) this
        else new Map3(key1, kv._2, key2, value2, key3, value3)
      } else if (key2 == kv._1) {
        if (value2 == kv._2) this
        else new Map3(key1, value1, key2, kv._2, key3, value3)
      } else if (key3 == kv._1) {
        if (value3 == kv._2) this
        else new Map3(key1, value1, key2, value2, key3, kv._2)
      } else {
        new Map4(key1, value1, key2, value2, key3, value3, kv._1, kv._2)
      }
    }
    override def -(k: A) = {
      if (k == key1) {
        new Map2(key2, value2, key3, value3)
      } else if (k == key2) {
        new Map2(key1, value1, key3, value3)
      } else if (k == key3) {
        new Map2(key1, value1, key2, value2)
      } else {
        this
      }
    }
    override def get(k: A) = {
      if (k == key1) {
        Some(value1)
      } else if (k == key2) {
        Some(value2)
      } else if (k == key3) {
        Some(value3)
      } else {
        None
      }
    }
    override def iterator = Iterator((key1, value1), (key2, value2), (key3, value3))
    override def keySet = new SmallSets.Set3(key1, key2, key3)
    override def filter(p: ((A, B)) => Boolean) = {
      (p((key1, value1)), p((key2, value2)), p(key3, value3)) match {
        case (true, true, true) => this
        case (true, true, false) => new Map2(key1, value1, key2, value2)
        case (true, false, true) => new Map2(key1, value1, key3, value3)
        case (true, false, false) => new Map1(key1, value1)
        case (false, true, true) => new Map2(key2, value2, key3, value3)
        case (false, true, false) => new Map1(key2, value2)
        case (false, false, true) => new Map1(key3, value3)
        case (false, false, false) => SmallMaps.empty
      }
    }
    override def filterKeys(p: A => Boolean) = {
      (p(key1), p(key2), p(key3)) match {
        case (true, true, true) => this
        case (true, true, false) => new Map2(key1, value1, key2, value2)
        case (true, false, true) => new Map2(key1, value1, key3, value3)
        case (true, false, false) => new Map1(key1, value1)
        case (false, true, true) => new Map2(key2, value2, key3, value3)
        case (false, true, false) => new Map1(key2, value2)
        case (false, false, true) => new Map1(key3, value3)
        case (false, false, false) => SmallMaps.empty
      }
    }
    override def mapValues[C](f: B => C) = new Map3(key1, f(value1), key2, f(value2), key3, f(value3))
    override def contains(key: A) = key == key1 || key == key2 || key == key3
    override def foreach[U](f: ((A, B)) => U) {
      f((key1, value1))
      f((key2, value2))
      f((key3, value3))
    }
  }
  class Map4[A, +B](key1: A, value1: B, key2: A, value2: B, key3: A, value3: B, key4: A, value4: B) extends Map[A, B] with SmallNonEmptyMap[A, B] {
    override def -(k: A) = {
      if (k == key1) {
        new Map3(key2, value2, key3, value3, key4, value4)
      } else if (k == key2) {
        new Map3(key1, value1, key3, value3, key4, value4)
      } else if (k == key3) {
        new Map3(key1, value1, key2, value2, key4, value4)
      } else if (k == key4) {
        new Map3(key1, value1, key2, value2, key3, value3)
      } else {
        this
      }
    }
    override def get(k: A) = {
      if (k == key1) {
        Some(value1)
      } else if (k == key2) {
        Some(value2)
      } else if (k == key3) {
        Some(value3)
      } else if (k == key4) {
        Some(value4)
      } else {
        None
      }
    }
    override def iterator = Iterator((key1, value1), (key2, value2), (key3, value3), (key4, value4))
    override def keySet = new SmallSets.Set4(key1, key2, key3, key4)
    override def mapValues[C](f: B => C) = new Map4(key1, f(value1), key2, f(value2), key3, f(value3), key4, f(value4))
    override def contains(key: A) = key == key1 || key == key2 || key == key3 || key == key4
    override def foreach[U](f: ((A, B)) => U) {
      f((key1, value1))
      f((key2, value2))
      f((key3, value3))
      f((key4, value4))
    }
  }
}
object SmallSets {
  def empty[A] = EmptySet.asInstanceOf[Set[A]]

  object EmptySet extends Set[Nothing] {
    override def contains(a: Nothing) = false
    override def iterator = Iterator.empty
    override def -(elem: Nothing) = Set.empty.-(elem)
    override def +(elem: Nothing) = new Set1(elem)
    override def filter(p: Nothing => Boolean) = this
    override def map[B, That](f: Nothing ⇒ B)(implicit bf: CanBuildFrom[Set[Nothing], B, That]): That = bf().result()
    override def foreach[U](f: Nothing => U) {}
    override def isEmpty = true
    override def nonEmpty = false
  }

  protected trait SmallNonEmptySet[A] { this: Set[A] =>
    override def isEmpty = false
    override def nonEmpty = true
  }

  class Set1[A](elem1: A) extends Set[A] with SmallNonEmptySet[A] {
    override def contains(a: A) = elem1 == a
    override def iterator = Iterator(elem1)
    override def -(elem: A) = {
      if (elem == elem1) {
        SmallSets.empty
      } else {
        this
      }
    }
    override def +(elem: A) = {
      if (elem == elem1) {
        this
      } else {
        new Set2(elem1, elem)
      }
    }
    override def filter(p: A => Boolean) = {
      if (p(elem1)) {
        this
      } else
        SmallSets.empty
    }
    override def map[B, That](f: A ⇒ B)(implicit bf: CanBuildFrom[Set[A], B, That]): That = ???
    override def foreach[U](f: A => U) {
      f(elem1)
    }
  }
  class Set2[A](elem1: A, elem2: A) extends Set[A] with SmallNonEmptySet[A] {
    override def contains(a: A) = elem1 == a || elem2 == a
    override def iterator = Iterator(elem1, elem2)
    override def -(elem: A) = {
      if (elem == elem1) {
        new Set1(elem2)
      } else if (elem == elem2) {
        new Set1(elem1)
      } else {
        this
      }
    }
    override def +(elem: A) = {
      if (elem == elem1 || elem == elem2) {
        this
      } else {
        ??? // new Set3(elem1, elem2, elem)
      }
    }
    override def filter(p: A => Boolean) = {
      (p(elem1), p(elem2)) match {
        case (true, true) => this
        case (true, false) => new Set1(elem1)
        case (false, true) => new Set1(elem2)
        case (false, false) => SmallSets.empty
      }
    }
    override def map[B, That](f: A ⇒ B)(implicit bf: CanBuildFrom[Set[A], B, That]): That = ???
    override def foreach[U](f: A => U) {
      f(elem1)
      f(elem2)
    }
  }
  class Set3[A](elem1: A, elem2: A, elem3: A) extends Set[A] with SmallNonEmptySet[A] {
    override def contains(a: A) = elem1 == a || elem2 == a || elem3 == a
    override def iterator = Iterator(elem1, elem2, elem3)
    override def -(elem: A) = {
      if (elem == elem1) {
        new Set2(elem2, elem3)
      } else if (elem == elem2) {
        new Set2(elem1, elem3)
      } else if (elem == elem3) {
        new Set2(elem1, elem2)
      } else {
        this
      }
    }
    override def +(elem: A) = {
      if (elem == elem1 || elem == elem2 || elem == elem3) {
        this
      } else {
        new Set4(elem1, elem2, elem3, elem)
      }
    }
    override def filter(p: A => Boolean) = {
      (p(elem1), p(elem2)) match {
        case (true, true, true) => this
        case (true, true, false) => new Set2(elem1, elem2)
        case (true, false, true) => new Set2(elem1, elem3)
        case (true, false, false) => SmallSets.empty
        case (false, true, true) => this
        case (false, true, false) => new Set1(elem1)
        case (false, false, true) => new Set1(elem2)
        case (false, false, false) => SmallSets.empty
      }
    }
    override def map[B, That](f: A ⇒ B)(implicit bf: CanBuildFrom[Set[A], B, That]): That = ???
    override def foreach[U](f: A => U) {
      f(elem1)
      f(elem2)
    }
  }
}