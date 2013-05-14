package epic.util

import java.io.IOException
import java.util.Collections
import com.google.common.collect.MapMaker

/**
 * just a simple in-memory cache with soft values
 * @author dlwh
 */
@SerialVersionUID(1L)
class Cache[K, V] extends Serializable with (K=>Option[V]) {
  @transient
  private var cache = Collections.synchronizedMap(new MapMaker().softValues().makeMap[K, V]())

  def apply(k: K) = {
    Option(cache.get(k))
  }

  def update(k: K, v: V) = {
    cache.put(k, v)
  }

  def getOrElseUpdate(k: K, v: =>V) = {
    apply(k).getOrElse {
      val x = v
      cache.put(k, x)
      x
    }
  }

  def forFunction(f: K=>V) = { (k: K) =>
    getOrElseUpdate(k, f(k))
  }

  def clear() {
    cache.clear()
  }


  @throws[IOException]
  @throws[ClassNotFoundException]
  private def readObject(stream: java.io.ObjectInputStream) {
    stream.defaultReadObject()
    cache = Collections.synchronizedMap(new MapMaker().softValues().makeMap[K, V]())
  }
}
