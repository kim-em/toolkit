package net.tqft.toolkit.collections

trait Stack[A] {
  def push(a: A)
  def pop: Option[A]
  def clear
}

object Stack {
  implicit def stack2RichStack[A](s: Stack[A]) = new RichStack(s)
  
  class RichStack[A](s: Stack[A]) {
    def transform[B](f: A => B, g: B => A): Stack[B] = new Stack[B] {
      def push(b: B) = s.push(g(b))
      def pop = s.pop.map(f(_))
      def clear = s.clear
    }
    
    def asQueue: Queue[A] = new Queue[A] {
      def enqueue(a: A) = s.push(a)
      def dequeue = s.pop
    }
  }
}