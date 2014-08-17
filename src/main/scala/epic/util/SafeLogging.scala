package epic.util

import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

/**
 * Stupid Typesafe logging lib trait isn't serializable. This is just a better version.
 *
 * @author dlwh
 **/
trait SafeLogging {
  @transient @volatile
  private var _the_logger:Logger = null

  def logger: Logger = {
    var logger = _the_logger
    if(logger eq null) {
      synchronized {
        logger = _the_logger
        if(logger eq null) {
          val ll = Logger(LoggerFactory.getLogger(this.getClass))
          _the_logger = ll
          logger = ll
        }
      }
    }
    logger
  }


}
