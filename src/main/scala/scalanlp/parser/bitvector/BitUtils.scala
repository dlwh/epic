/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package scalanlp.parser.bitvector

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
}
