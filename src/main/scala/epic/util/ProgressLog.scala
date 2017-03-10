package epic.util

import java.util.concurrent.atomic.AtomicInteger

import breeze.util.LazyLogger
import org.slf4j.Logger

/**
 *
 *
 * @author dlwh
 */
class ProgressLog(log: LazyLogger, items: Int, frequency: Int = 100, name: String = "Progress") {
  val initialTime = System.currentTimeMillis()
  val item = new AtomicInteger()

  def reportProgress() = {
    val x = item.incrementAndGet()
    if (x % frequency == 0 || x == items) {
      log.info(s"$name $x/$items (${(System.currentTimeMillis() - initialTime)/1000.0}s elapsed.)")
    }
  }

  def info(msg: =>String) = {
    val x = item.incrementAndGet()
    if (x % frequency == 0 || x == items) {
      val m = msg
      log.info(s"$name $x/$items: $m (${(System.currentTimeMillis() - initialTime)/1000.0}s elapsed.)")
    }
  }

  def debug(msg: =>String) = {
    val x = item.incrementAndGet()
    if (x % frequency == 0 || x == items) {
      val m = msg
      log.debug(s"$name $x/$items: $m (${(System.currentTimeMillis() - initialTime)/1000.0}s elapsed.)")
    }
  }

}
