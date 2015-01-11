//package epic.corefdense
//
//class OffsetArray(val values: Array[Float],
//                  val offset: Int) {
//  def this(values: Array[Float]) = this(values, 0)
//
//  def read(idx: Int) = values(offset + idx)
//  
//  def write(idx: Int, value: Float) {
//    values(offset + idx) = value
//  }
//  
//  def increment(idx: Int, value: Float) {
//    write(idx, read(idx) + value)
//  }
//  
//  def scoreFeats(feats: Array[Int]): Float = {
//    var total = 0.0F;
//    var i = 0;
//    while (i < feats.size) {
//      total += read(feats(i))
//      i += 1;
//    }
//    total
//  }
//  
//  def addOffset(idx: Int) = {
//    new OffsetArray(values, offset + idx)
//  }
//}