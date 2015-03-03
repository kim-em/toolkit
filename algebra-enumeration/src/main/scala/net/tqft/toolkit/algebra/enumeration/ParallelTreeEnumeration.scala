package net.tqft.toolkit.algebra.enumeration

import scala.collection.mutable.Queue
import java.util.concurrent.BlockingQueue
import java.util.concurrent.LinkedBlockingQueue
import scala.concurrent.Future
import scala.collection.immutable.BitSet

trait TreeConsumer[A] {
  def put(a: A)
  def complete(res: Int, mod: Int)
  def finished_? : Boolean
}

case class MockConsumer[A](var finished_? : Boolean) extends TreeConsumer[A] {
  println("Creating new MockConsumer.")
  def put(a: A) = println("Recording " + a)
  def complete(res: Int, mod: Int) {
    finished_? = true
    println(s"Completed residue class $res/$mod.")
  }
}

case class ParallelTreeEnumeration[A](children: A => List[A], newConsumer: () => TreeConsumer[A], runtimeBoundSeconds: Long) {

  def enumerate(root: A, res: Int, mod: Int) {
    val queue = new LinkedBlockingQueue[Either[Progress, TreeConsumer[A]]]()
    val consumers = scala.collection.mutable.Set.empty[TreeConsumer[A]]

    def unfinishedConsumer: Option[TreeConsumer[A]] = {
      consumers.retain(c => !c.finished_?)
      consumers.headOption
    }

    def createAndRegisterConsumer = {
      val c = newConsumer()
      consumers += c
      c
    }

    queue.put(Left(Progress(List(List((root, BitSet((0 until mod): _*)))), res, mod, createAndRegisterConsumer)))

    while (queue.peek != null || unfinishedConsumer.nonEmpty) {
      queue.take match {
        case Left(p) => {
          import scala.concurrent.ExecutionContext.Implicits.global
          Future {
            p.work
          }
        }
        case Right(c) => {}
      }

    }

    case class Progress(var stack: List[List[(A, BitSet)]], var res: Int, var mod: Int, consumer: TreeConsumer[A]) {
      val startTime = System.nanoTime
      def elapsedTime = (System.nanoTime - startTime) / 1000000000

      def work {
        while (stack.nonEmpty && elapsedTime < runtimeBoundSeconds) {
          stack match {
            case ((a, residues) :: others) :: tail => {
              if (res == residues.min) {
                consumer.put(a)
              }
              if(residues.contains(res)) {
                val c = children(a)
                val pairs = if(residues.size > c.size) {
                  ??? 
                } else {
                  ???
                }
                stack = pairs :: others :: tail
              } else {
                stack = others :: tail
              }
            }
            case Nil :: tail => stack = tail
            case Nil => throw new Error
          }
        }

        if (stack.isEmpty) {
          // report completion to the consumer
          consumer.complete(res, mod)
          queue.put(Right(consumer))
        } else {
          // split and push both pieces to the queue
          val first = Progress(???, res, 2 * mod, consumer)
          val second = Progress(???, res + mod, 2 * mod, createAndRegisterConsumer)
          queue.put(Left(first))
          queue.put(Left(second))
        }
      }
    }
  }
}
