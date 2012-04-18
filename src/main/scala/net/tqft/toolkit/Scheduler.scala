package net.tqft.toolkit

object Scheduler extends Logging {
  import scala.actors.{ Actor, TIMEOUT }
  def apply(time: Long)(f: => Unit) = {
    def fixedRateLoop {
      Actor.reactWithin(time) {
        case TIMEOUT =>
          try {
            f
          } catch {
            case e => error("Caught an exception while running a task: ", e)
          }
          fixedRateLoop
        case 'stop =>
      }
    }
    Actor.actor(fixedRateLoop)
  }

}