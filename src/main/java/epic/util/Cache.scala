package epic.util

import java.io.{File, IOException}
import java.util.Collections
import org.mapdb.DBMaker
import scala.collection.JavaConverters._
import scala.collection.concurrent.Map

case class CacheBroker(path: File = null, autocommit: Boolean = true, disableWriteAheadLog: Boolean = false) {
  val dbMaker = if(path eq null) {
    DBMaker.newMemoryDB()
  } else {
    DBMaker.newFileDB(path)
  }.closeOnJvmShutdown().cacheSoftRefEnable()
  if(disableWriteAheadLog) dbMaker.writeAheadLogDisable()

  def commit() { db.commit()}
  def close() {db.close()}

  lazy val db = {if(autocommit) cacheThread.start(); dbMaker.make()}
  def make[K,V](name: String): Map[K, V] = db.getHashMap[K, V](name).asScala

  private lazy val cacheThread: Thread = new Thread(new Runnable {
    def run() {
      while(true) {
        Thread.sleep(1000 * 60 * 5)
        db.commit()
      }
    }
  }) {
    setDaemon(true)
  }
}
