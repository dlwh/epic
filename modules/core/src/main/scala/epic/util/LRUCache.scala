package epic.util

import scala.reflect.ClassTag
import scala.util.hashing.MurmurHash3

/**
 * Well, an approximate LRU cache base on hashing
 *
 * @author dlwh
 **/
final class LRUCache[@specialized(Int, Long) K:ClassTag, V:ClassTag](size: Int, onEvict: (K, V)=>Unit = (k: K, v: V) => ()) {
  private val keys = new Array[K](size)
  private val occupied = new Array[Int](size)
  java.util.Arrays.fill(occupied, -1)
  private var nextKey = 0
  private val values = new Array[V](size)

  def get(k: K):Option[V] = {
    val pos = lookup(k)
    if (occupied(pos) >= 0 && keys(pos) == k) {
      occupied(pos) = nextKey
      nextKey += 1
      Some(values(pos))
    } else {
      None
    }
  }

  def -=(k: K) = {
    val pos = lookup(k)
    if (occupied(pos) >= 0 && keys(pos) == k) {
      occupied(pos) = -1
      onEvict(keys(pos), values(pos))
    }
  }

  def iterator: Iterator[(K, V)] = {
    Iterator.range(0, size).filter(occupied(_) >= 0).map ( i => keys(i) -> values(i))
  }

  def getOrElseUpdate(k: K, v: =>V) = {
    val pos = lookup(k)
    if (occupied(pos) < 0 || keys(pos) != k) {
      if (occupied(pos) >= 0) {
        onEvict(keys(pos), values(pos))
      }
      keys(pos) = k
      values(pos) = v
    }
    occupied(pos) = nextKey
    nextKey += 1
    values(pos)
  }

  def update(k: K, v: V) {
    val pos = lookup(k)
    if (occupied(pos) >= 0) {
      onEvict(keys(pos), values(pos))
    }
    occupied(pos) = nextKey
    nextKey += 1
    keys(pos) = k
    values(pos) = v
  }

  private def lookup(k: K): Int = {
    val hc : Int = k.##
    val hh = MurmurHash3.mixLast(10891, hc).abs % keys.length
    val hh2 = MurmurHash3.mixLast(10909, hc).abs % keys.length
    if (occupied(hh) >= 0 && keys(hh) == k) {
      hh
    } else if (occupied(hh2) >= 0 && keys(hh2) == k) {
      hh2
    } else if (occupied(hh) <= occupied(hh2)) {
      hh
    } else {
      hh2
    }
  }

}
