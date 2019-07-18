package epic.util

import org.scalacheck.Prop
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

class LRUCacheTest extends FunSuite with Checkers {
  test("the key/value pair is always right") {

    this.check(Prop.forAll { (s: Map[Int, Long]) =>
      val lruCache = new LRUCache[Int, Long](10)
      (
        s.forall { case (k, v) =>
          lruCache(k) = v
          lruCache.get(k) === Some(v)
        }
        &&

        s.forall { case (k, v) =>
            lruCache.get(k).forall(_ == v)
        }
      )


    })
  }

  test("overriting does the right thing") {
    val lruCache = new LRUCache[Int, Long](10)
    lruCache(10) = 1000
    lruCache(10) = 1004
    assert(lruCache.get(10) === Some(1004))
  }

}
