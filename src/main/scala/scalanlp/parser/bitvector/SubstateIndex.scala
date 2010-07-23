package scalanlp.parser.bitvector

import scalanlp.util.Index

/**
 * A SubstateIndex maintains an index over (T,Int) pairs where the Ints are all
 * from 0 to N_t, for each object t in some original index.
 *
 * Not especially threadsafe.
 * 
 * @author dlwh
 */
class SubstateIndex[T](objs: Index[T], substatesForObj: T=>Int) extends Index[(T,Int)] {
  val (offsets:Array[Int],sz:Int,numSubstates: Array[Int]) = {
    val offsets = new Array[Int](objs.size);
    val numSubstates = new Array[Int](objs.size);
    var offset = 0;
    for((t,i) <- objs.zipWithIndex) {
      offsets(i) = offset;
      numSubstates(i) = substatesForObj(t);
      offset += numSubstates(i);
    }
    (offsets,offset,numSubstates);
  }
  
  def apply(t: (T, Int)): Int = {
    val tInd = objs(t._1);
    offsets(objs(t._1)) + t._2;
  }

  final override def size = sz;


  def unapply(i: Int) = if(i < 0 || i >= size) {
    None
  } else {
    val off = findOffset(i);
    // got lucky, it's the start of some offset.
    if(off >= 0) Some((objs.get(off),0))
    else {
      val ind = ~off - 1;
      val base = offsets(ind);
      val obj = objs.get(ind);
      val sub = i - base;
      Some((obj,sub));
    }
  }


  def iterator = for( (o,i) <- objs.pairs; s <- Iterator.range(0,numSubstates(i))) yield (o,s);

  def pairs = iterator.zipWithIndex

  private var lastOffset = 0;

  private def found(index : Int, offset : Int) : Int = {
    lastOffset = offset;
    offset;
  }

  /**
   * Returns the offset into index and data for the requested vector
   * index.  If the requested index is not found, the return value is
   * negative and can be converted into an insertion point with -(rv+1).
   */
  private def findOffset(i : Int) : Int = {
    val lastOffset = this.lastOffset;
    val lastIndex = offsets(lastOffset)

    if (i == lastIndex) {
      // previous element; don't need to update lastOffset
      lastOffset;
    } else {
      // regular binary search
      var begin = 0;
      var end = offsets.length - 1;

      // narrow the search if we have a previous reference
      if (lastIndex >= 0 && lastOffset >= 0) {
        if (i < lastIndex) {
          // in range preceding last request
          end = lastOffset;
        } else {
          // in range following last request
          begin = lastOffset;

          if (begin + 1 <= end && offsets(begin + 1) == i) {
            // special case: successor of last request
            return found(i, begin + 1);
          }
        }
      }

      // Simple optimization:
      // the i'th entry can't be after entry i.
      if(end > i)
        end = i;

      var mid = (end + begin) >> 1;

      while (begin <= end) {
        mid = (end + begin) >> 1;
        if (offsets(mid) < i)
          begin = mid + 1;
        else if (offsets(mid) > i)
          end = mid - 1;
        else
          return found(i, mid);
      }

      // no match found, return insertion point
      if (i <= offsets(mid))
        ~mid;     // Insert here (before mid)
      else
        ~(mid + 1); // Insert after mid
    }
  }
}