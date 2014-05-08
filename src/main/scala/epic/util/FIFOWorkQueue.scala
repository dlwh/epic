package epic.util

import scala.concurrent.{Await, Future, ExecutionContext}
import java.util.concurrent.ConcurrentLinkedQueue
import scala.concurrent.duration.Duration
import scala.annotation.unchecked.uncheckedVariance

/**
 * TODO
 *
 * @author dlwh
 **/
class FIFOWorkQueue[-In, Out](f: In=>Out)(implicit context: ExecutionContext) extends Iterator[Out] {

  private val queue = new ConcurrentLinkedQueue[Future[Out @uncheckedVariance ]]()
  private var done = false

  def +=(item: In) = enqueue(item)

  def ++=(items: TraversableOnce[In]) = items foreach +=

  def enqueue(item: In) {
    queue.add(Future(f(item)))
    synchronized {
      notifyAll()
    }
  }

  def finish() = synchronized {
    done = true
    notifyAll()
  }

  def hasNext = !queue.isEmpty || waitUntilReady()

  def next() = {waitUntilReady(); Await.result(queue.poll(), Duration.Inf)}

  private def waitUntilReady():Boolean = {
    synchronized {
      while(!done && queue.isEmpty) {
        wait()
      }
    }
    !queue.isEmpty || !done
  }

}

object FIFOWorkQueue {
  def apply[In, Out](iter: TraversableOnce[In])(f: In=>Out)(implicit context: ExecutionContext) = {
    val queue = new FIFOWorkQueue[In, Out](f)
    Future {
      queue ++= iter
      queue.finish()
    }
    queue
  }
}
