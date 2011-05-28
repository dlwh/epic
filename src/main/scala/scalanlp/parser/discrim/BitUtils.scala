package scalanlp.parser.discrim

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

object BitUtils {

  def iterateBits(field: Int, numBits: Int) = for ( b <- 0 until numBits iterator) yield {
    val m = 1 << b;
    val bit = if( (m & field) != 0) 1 else 0
    (b,bit);
  }

  def iterateBitIndices(field: Int, numBits: Int) = for ( b <- 0 until numBits iterator) yield {
    val m = 1 << b;
    val bit = if( (m & field) != 0) 1 else 0
    2 * b + bit;
  }

  def bitMatches(field: Int, bit: Int, set: Int) = ((field & (1 << bit)) > 0) == (set != 0);

  // from http://graphics.stanford.edu/~seander/bithacks.html#RoundUpPowerOf2Float
  def roundToNextPowerOfTwo(v: Int) = if(v < 1) 1 else {
    val f = v.asInstanceOf[Float]
    val t = 1 << ((java.lang.Float.floatToIntBits(f) >> 23) - 0x7f)
    t << (if(t < v) 1 else 0)
  }
}
