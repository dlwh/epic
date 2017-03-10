package epic.util

import java.io.File
import java.util.Collections
import org.mapdb.{Pump, Serializer, DBMaker}
import scala.collection.JavaConverters._
import scala.collection.concurrent.Map
import java.util

import scala.collection.{mutable, GenTraversableOnce}
import breeze.util.SerializableLogging

@SerialVersionUID(1L)
case class CacheBroker(path: File = null, copyFrom: File = null, clearCaches: String = "", autocommit: Boolean = true, disableWriteAheadLog: Boolean = false) extends Serializable {
  // this is how one makes a transient lazy val, sadly.
  @transient
  private var _actualCache:CacheBroker.ActualCache = null
  private def actualCache = synchronized {
    lazy val dbMaker = if (path eq null) {
      DBMaker.newMemoryDB()
    } else {
      DBMaker.newFileDB(path)
    }.closeOnJvmShutdown().cacheSoftRefEnable()

    if (_actualCache eq null) {
      _actualCache = CacheBroker.getCacheBroker(path, dbMaker, autocommit, copyFrom)
    }
    if (disableWriteAheadLog) _actualCache.dbMaker.writeAheadLogDisable()
    if (clearCaches != null && clearCaches.nonEmpty)
      for(toDisable <- clearCaches.split(",")) {
        _actualCache.db.getHashMap(toDisable).clear()
      }

    _actualCache
  }

  def dbMaker = actualCache.dbMaker
  def db = actualCache.db

  def commit() { db.commit()}
  def close() {actualCache.close()}

  def make[K,V](name: String)(implicit kser: Serializer[K] = null, vser: Serializer[V] = null): Map[K, V] = new CacheBroker.CacheMap[K, V](name, this)

}

object CacheBroker extends SerializableLogging {
  private class ActualCache private[CacheBroker] (val path: File, val dbMaker: DBMaker, val autocommit: Boolean, copyFrom: File = null) {
    lazy val db = {
      val db = dbMaker.make()
      if (copyFrom != null) {
        logger.info(s"Copying database from $copyFrom to ${if (path ne null) path else "in memory database"}")
        val from = DBMaker.newFileDB(copyFrom).make()
        Pump.copy(from, db)
        from.close()
      }
      if (autocommit) cacheThread.start()

      db
    }
    private lazy val cacheThread: Thread = new Thread(new Runnable {
      def run() {
        try {
          while (!db.isClosed && !Thread.interrupted()) {
            Thread.sleep(1000 * 60)
            if (!db.isClosed)
              db.commit()
          }
        } catch {
          case ex: InterruptedException =>
          case ex: IllegalAccessError =>
        }
      }
    }) {
      setDaemon(true)
    }

    def close() {
      db.close()
      cacheThread.interrupt()
      if (path ne null)
        cacheCache -= path
    }
  }

  private val cacheCache = Collections.synchronizedMap(new util.HashMap[File, ActualCache]()).asScala

  private def getCacheBroker(path: File, dbMaker: =>DBMaker, autocommit: Boolean, copyFrom: File) = {
    if (path eq null) new ActualCache(path, dbMaker, autocommit)
    else cacheCache.getOrElseUpdate(path, new ActualCache(path, dbMaker, autocommit, copyFrom))
  }

  @SerialVersionUID(1L)
  private class CacheMap[K, V](name: String, cache: CacheBroker)(implicit kser: Serializer[K], vser: Serializer[V]) extends Map[K, V] with Serializable {
    import cache._
    @transient
    private var _theMap : Map[K, V] = null
    def theMap = synchronized {
      if (_theMap eq null) {
        _theMap = try {
          // this throws if the hash map exists, and there's no "does it exist" method
          // that takes the serializers...
          db.createHashMap[K, V](name, true, kser, vser).asScala
        } catch {
          case ex: IllegalArgumentException => db.getHashMap[K, V](name).asScala
        }
      }
      _theMap
    }

    def +=(kv: (K, V)): this.type = {theMap += kv; this}

    def -=(key: K): this.type = {theMap -= key; this}

    def get(key: K): Option[V] = theMap.get(key: K)

    def iterator: Iterator[(K, V)] = theMap.iterator

    def putIfAbsent(k: K, v: V): Option[V] = theMap.putIfAbsent(k: K, v: V)

    def remove(k: K, v: V): Boolean = theMap.remove(k: K, v: V)

    def replace(k: K, oldvalue: V, newvalue: V): Boolean = theMap.replace(k: K, oldvalue: V, newvalue: V)

    def replace(k: K, v: V): Option[V] = theMap.replace(k: K, v: V)

    override def size: Int =  theMap.size

    override def put(key: K, value: V): Option[V] = theMap.put(key: K, value: V): Option[V]

    override def update(key: K, value: V) {
      theMap.update(key, value)
    }

    override def updated[B1 >: V](key: K, value: B1): mutable.Map[K, B1] = theMap.updated(key, value)

    override def +[B1 >: V](kv: (K, B1)): mutable.Map[K, B1] = theMap.+(kv)

    override def +[B1 >: V](elem1: (K, B1), elem2: (K, B1), elems: (K, B1)*): mutable.Map[K, B1] = theMap.+(elem1, elem2, elems:_*)

    override def ++[B1 >: V](xs: GenTraversableOnce[(K, B1)]): mutable.Map[K, B1] = theMap.++(xs)

    override def remove(key: K): Option[V] = theMap.remove(key)

    override def -(key: K): mutable.Map[K, V] = theMap.-(key)

    override def clear() {
      theMap.clear()
    }

    override def getOrElseUpdate(key: K, op: => V): V = theMap.getOrElseUpdate(key, op)

    override def transform(f: (K, V) => V): this.type = {theMap.transform(f);this}

    override def retain(p: (K, V) => Boolean): this.type = {theMap.retain(p); this}

    override def clone(): mutable.Map[K, V] = theMap.clone()

    override def result(): mutable.Map[K, V] = theMap.result()

    override def -(elem1: K, elem2: K, elems: K*): mutable.Map[K, V] = theMap.-(elem1, elem2, elems:_*)

    override def --(xs: GenTraversableOnce[K]): mutable.Map[K, V] = theMap.--(xs)

  }
}
