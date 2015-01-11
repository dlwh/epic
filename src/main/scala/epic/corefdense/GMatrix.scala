//package epic.corefdense
//
//import edu.berkeley.nlp.futile.util.Logger
//
//// Column-major
//class GMatrix(rows: Int, cols: Int) {
//  
//  def size = rows * cols
//
//  def getIdx(rowIdx: Int, colIdx: Int) = colIdx * rows + rowIdx;
//  
//  def mmul(weights: Array[Float], vec: Array[Float], dest: Array[Float]) {
//    mmul(new OffsetArray(weights, 0), vec, dest)
//  }
//  
//  def mmul(weights: OffsetArray, vec: Array[Float], dest: Array[Float]) {
//    var i = 0;
//    while (i < rows) {
//      dest(i) = 0;
//      var j = 0;
//      while (j < cols) {
//        val idx = getIdx(i, j)
//        dest(i) += vec(j) * weights.read(idx)
//        j += 1;
//      }
//      i += 1;
//    }
//  }
//  
//  def toString(weights: Array[Float]) = {
//    var sb = new StringBuilder
//    for (i <- 0 until rows) {
//      for (j <- 0 until cols) {
//        sb = sb.append(weights(getIdx(i, j))).append(" ")
//      }
//      sb = sb.append("\n")
//    }
//    sb.toString
//  }
//  
//  def getQuasiIdentityWeights: Array[Float] = {
//    val weights = new Array[Float](size)
//    if (cols > rows) {
//      val numPer = cols/rows
//      val switchPoint = cols % rows;
//      var progress = 0;
//      var i = 0;
//      while (i < rows) {
//        var j = 0;
//        var numOnesThisRow = numPer + (if (i < switchPoint) 1 else 0)
//        while (j < cols) {
//          if (j >= progress && j < progress + numOnesThisRow) {
//            weights(getIdx(i, j)) = 1.0F
//          } else {
//            weights(getIdx(i, j)) = 0.0F
//          }
//          j += 1
//        }
//        i += 1
//        progress += numOnesThisRow;
//      }
//    } else {
//      val numPer = rows/cols
//      val switchPoint = rows % cols;
//      var progress = 0;
//      var j = 0;
//      while (j < cols) {
//        var i = 0;
//        var numOnesThisCol = numPer + (if (i < switchPoint) 1 else 0)
//        while (i < rows) {
//          if (i >= progress && i < progress + numOnesThisCol) {
//            weights(getIdx(i, j)) = 1.0F
//          } else {
//            weights(getIdx(i, j)) = 0.0F
//          }
//          i += 1
//        }
//        j += 1
//        progress += numOnesThisCol;
//      }
//    }
//    weights
//    
//  }
//}
//
//object GMatrix {
//  
//  def main(args: Array[String]) {
//    val gm = new GMatrix(3, 5)
//    println(gm.toString(gm.getQuasiIdentityWeights))
//    val gm2 = new GMatrix(5, 3)
//    println(gm2.toString(gm2.getQuasiIdentityWeights))
//  }
//}