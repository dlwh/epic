package epic.corefdense

import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.util.zip.GZIPInputStream
import java.util.zip.GZIPOutputStream
import scala.collection.JavaConverters.iterableAsScalaIterableConverter
import scala.collection.mutable.ArrayBuffer
import scala.util.Sorting
import edu.berkeley.nlp.futile.math.SloppyMath
import edu.berkeley.nlp.futile.util.Counter
import edu.berkeley.nlp.futile.util.Iterators
import edu.berkeley.nlp.futile.util.Logger
import edu.berkeley.nlp.futile.fig.basic.Indexer

/**
 * Miscellaneous utilities that are used in a variety of classes
 */
object GUtil {
  
  def save[T <: Serializable](obj: T, path: String) {
    if (!new File(path).getParentFile().canWrite()) {
      throw new RuntimeException("Can't write to " + path); 
    }
    if (path.endsWith(".gz")) saveGz(obj, path) else saveNonGz(obj, path);
  }
  
  def load(path: String): Object = {
    if (!new File(path).canRead()) {
      throw new RuntimeException("Can't read from " + path); 
    }
    if (path.endsWith(".gz")) loadGz(path) else loadNonGz(path);
  }
  
  def saveNonGz[T <: Serializable](obj: T, path: String) {
    try {
      val fileOut = new FileOutputStream(path);
      val out = new ObjectOutputStream(fileOut);
      out.writeObject(obj);
      Logger.logss("Wrote to " + path);
      out.close();
      fileOut.close();
    } catch {
      case e: Exception => throw new RuntimeException(e);
    }
  }
  
  def loadNonGz(path: String): Object = {
    var obj: Object = null;
    try {
      val fileIn = new FileInputStream(new File(path));
      val in = new ObjectInputStream(fileIn);
      obj = in.readObject()
      Logger.logss("Object read from " + path);
      in.close();
      fileIn.close();
    } catch {
      case e: Exception => throw new RuntimeException(e);
    }
    obj;
  }
  
  def saveGz[T <: Serializable](obj: T, path: String) {
    try {
      val fileOut = new BufferedOutputStream(new FileOutputStream(path));
      val gzOutputStream = new GZIPOutputStream(fileOut);
      val out = new ObjectOutputStream(gzOutputStream);
      out.writeObject(obj);
      Logger.logss("Wrote to " + path);
      out.close();
    } catch {
      case e: Exception => throw new RuntimeException(e);
    }
  }
  
  def loadGz(path: String): Object = {
    var obj: Object = null;
    try {
      val fileIn = new BufferedInputStream(new FileInputStream(path));
      val gzipInputStream = new GZIPInputStream(fileIn);
      val in = new ObjectInputStream(gzipInputStream);
      obj = in.readObject()
      Logger.logss("Object read from " + path);
      in.close();
    } catch {
      case e: Exception => throw new RuntimeException(e);
    }
    obj;
  }
  
  def fmt(mat: Array[Array[Double]]): String = {
    var str = "";
    for (i <- 0 until mat.size) {
      for (j <- 0 until mat(i).size) {
        str += GUtil.fmt(mat(i)(j)) + "\t";
      }
      str += "\n";
    }
    str;
  }
  
//  def fmt(col: Collection[Double]): String = {
//    if (col.size == 0) {
//      "[]"
//    } else {
//      "[" + col.foldLeft("")((curr, nextD) => curr + fmt(nextD) + ", ").dropRight(2) + "]";
//    }
//  }
  
  def fmt(d: Double): String = {
    if (d.isNaN) {
      "NaN";
    } else if (d.isPosInfinity) {
      "+Inf";
    } else if (d.isNegInfinity) {
      "-Inf";
    } else {
      if (d < 0) "-" + fmtPositiveNumber(-d) else fmtPositiveNumber(d);
    }
  }
  
  def fmtProb(d: Double): String = {
    fmtPositiveNumber(d);
  }
  
  def fmtPositiveNumber(d: Double): String = {
    require(d >= 0);
    if (d == 0) {
      "0";
    }
    if (d < 1e-20) {
      "tiny"
    } else if (d < 0.001) {
      val numPlacesToMove = Math.ceil(-Math.log(d)/Math.log(10)).toInt;
      "%1.1f".format(d * Math.pow(10, numPlacesToMove)) + "e-" + numPlacesToMove; 
    } else if (d < 10000) {
      "%1.3f".format(d);
    } else {
      val numPlacesToMove = Math.floor(Math.log(d)/Math.log(10)).toInt;
      "%1.1f".format(d / Math.pow(10, numPlacesToMove)) + "e" + numPlacesToMove;
    }
  }
  
  def fmtTwoDigitNumber(d: Double, numDecimalPlaces: Int): String = {
    ("%1." + numDecimalPlaces + "f").format(d);
  }
  
  def containsNaN(array: Array[Double]): Boolean = {
    var containsNaN = false;
    var idx = 0;
    while (idx < array.size) {
      containsNaN = containsNaN || array(idx).isNaN;
      idx += 1;
    }
    containsNaN;
  }
  
  def containsVariousValues(array: Array[Double], homeValue: Double): Boolean = {
    var containsVarious = false;
    var idx = 0;
    while (idx < array.size) {
      containsVarious = containsVarious || Math.abs(array(idx) - homeValue) > 0.1;
      idx += 1;
    }
    containsVarious;
  }
  
  def containsNaNOrNegInf(array: Array[Double]): Boolean = {
    var bad = false;
    for (value <- array) {
      bad = bad || value.isNaN || value.isNegInfinity;
    }
    bad;
  }
  
  def containsNaNOrInf(array: Array[Double]): Boolean = {
    var bad = false;
    for (value <- array) {
      bad = bad || value.isNaN || value.isInfinite;
    }
    bad;
  }

  def getNBest[A](stuff: Seq[A], scorer: (A) => Double, n: Int): Seq[(A, Double)] = {
    val counter = new Counter[A]();
    for (thing <- stuff) {
      counter.setCount(thing, scorer(thing));
    }
    val results = new ArrayBuffer[(A, Double)]();
    for (thing <- Iterators.able(counter.asPriorityQueue()).asScala) {
      if (results.size < n) {
        results += new Tuple2(thing, counter.getCount(thing));
      }
    }
    results;
  }
  
  def getTopNKeysSubCounter[A](counter: Counter[A], n: Int) = {
    val newCounter = new Counter[A]();
    val pq = counter.asPriorityQueue()
    var numPrinted = 0;
    while (pq.hasNext() && numPrinted < n) {
      val obj = pq.next();
      newCounter.setCount(obj, counter.getCount(obj));
      numPrinted += 1;
    }
    newCounter;
  }
  
  def getKeysInOrder[A](counter: Counter[A]): Seq[A] = {
    val pq = counter.asPriorityQueue();
    val seq = new ArrayBuffer[A];
    while (pq.hasNext) {
      seq += pq.next;
    }
    seq;
  }
  
  def normalizeiSoft(arr: Array[Double]): Boolean = {
    var idx = 0;
    var total = 0.0;
    while (idx < arr.size) {
      total += arr(idx);
      idx += 1;
    }
    if (total <= 0.0) {
      false;
    } else {
      idx = 0;
      while (idx < arr.size) {
        arr(idx) /= total;
        idx += 1;
      }
      true;
    }
  }
  
  def normalizeiHard(arr: Array[Double]) {
    var idx = 0;
    var total = 0.0;
    while (idx < arr.size) {
      total += arr(idx);
      idx += 1;
    }
    if (total <= 0.0) {
      throw new RuntimeException("Bad total for normalizing: " + total);
    }
    idx = 0;
    while (idx < arr.size) {
      arr(idx) /= total;
      idx += 1;
    }
  }
  
  def normalizeiHard(arr: Array[Float]) {
    var idx = 0;
    var total = 0.0F;
    while (idx < arr.size) {
      total += arr(idx);
      idx += 1;
    }
    if (total <= 0.0F) {
      throw new RuntimeException("Bad total for normalizing: " + total);
    }
    idx = 0;
    while (idx < arr.size) {
      arr(idx) /= total;
      idx += 1;
    }
  }
  
  def expAndNormalizeiHard(arr: Array[Float]) {
    var idx = 0;
    while (idx < arr.size) {
      arr(idx) = Math.exp(arr(idx)).toFloat;
      idx += 1;
    }
    normalizeiHard(arr);
  }
  
  def expAndNormalizeiHard(arr: Array[Double]) {
    var idx = 0;
    while (idx < arr.size) {
      arr(idx) = Math.exp(arr(idx));
      idx += 1;
    }
    normalizeiHard(arr);
  }
  
  def renderMat[A](mat: Array[Array[A]]): String = {
    mat.map(row => row.map(_.toString).reduce((c1, c2) => c1 + ", " + c2)).reduce((r1, r2) => r1 + "\n" + r2);
  }
  
  def dot(vector1: Array[Float], vector2: Array[Float]) = {
    require(vector1.size == vector2.size, vector1.size + " " + vector2.size);
    var i = 0;
    var result = 0.0F;
    while (i < vector1.size) {
      result += vector1(i) * vector2(i);
      i += 1;
    }
    result;
    
  }
  
  def normalizei(vector: Array[Float]) {
    val normalizer = vector.reduce(_ + _);
    for (i <- 0 until vector.size) {
      vector(i) /= normalizer;
    }
  }
  
  def normalizei(vector: Array[Double]) {
    val normalizer = vector.reduce(_ + _);
    for (i <- 0 until vector.size) {
      vector(i) /= normalizer;
    }
  }
  
  def logNormalizei(vector: Array[Float]) {
    val normalizer = SloppyMath.logAdd(vector);
    for (i <- 0 until vector.size) {
      vector(i) = (vector(i) - normalizer).toFloat;
    }
  }
  
  def logNormalizei(vector: Array[Double]) {
    val normalizer = SloppyMath.logAdd(vector);
    for (i <- 0 until vector.size) {
      vector(i) -= normalizer;
    }
  }
  
  def logNormalizeiByRow(mat: Array[Array[Double]]) {
    for (i <- 0 until mat.size) {
      val normalizer = SloppyMath.logAdd(mat(i));
      for (j <- 0 until mat(i).size) {
        mat(i)(j) -= normalizer;
      }
    }
  }
  
  def computeQuantile(nums: Array[Double], quantile: Double): Double = {
    val numsCpy = new Array[Double](nums.size);
    Array.copy(nums, 0, numsCpy, 0, nums.size);
    Sorting.quickSort(numsCpy);
    numsCpy((quantile * nums.size).toInt);
  }
  
  def renderNumerDenom(numer: Double, denom: Double) = {
    numer + " / " + denom + " = " + (numer/denom);
  }
  
  def renderNumerDenom(numer: Int, denom: Int) = {
    numer + " / " + denom + " = " + (numer.toDouble/denom.toDouble);
  }
  
  def renderStandardPercentage(numer: Int, denom: Int) = {
    numer + " / " + denom + " = " + GUtil.fmtTwoDigitNumber(numer.toDouble/denom.toDouble * 100, 2);
  }
  
  def computeF1(corr: Int, precDenom: Int, recDenom: Int) = {
    val prec = corr.toDouble/precDenom.toDouble;
    val rec = corr.toDouble/recDenom.toDouble;
    2 * prec * rec / (prec + rec);
  }
  
  def renderPRF1(corr: Int, precDenom: Int, recDenom: Int) = {
    GUtil.renderNumerDenom(corr, precDenom)
    "Prec = " + GUtil.renderStandardPercentage(corr, precDenom) + ", Rec = " +
                GUtil.renderStandardPercentage(corr, recDenom) + ", F1 = " +
                GUtil.fmtTwoDigitNumber(computeF1(corr, precDenom, recDenom) * 100, 2) 
  }
  
  def padToK(str: String, k: Int) = {
    var newStr = str;
    while (newStr.size < k) {
      newStr += " ";
    }
    newStr;
  }
  
  def addToGradient(feats: Seq[Int], scale: Float, gradient: Array[Float]) {
    var i = 0;
    while (i < feats.size) {
      val feat = feats(i);
      gradient(feat) += scale;
      i += 1;
    }
  }
  
  def addToGradient(feats: Array[Int], scale: Float, gradient: Array[Float]) {
    var i = 0;
    while (i < feats.size) {
      val feat = feats(i);
      gradient(feat) += scale;
      i += 1;
    }
  }
  
  def argMaxIdxFloat(values: Seq[Float]) = {
    var currIdx = 0;
    var maxIdx = 0;
    var maxVal = Double.NegativeInfinity;
    while (currIdx < values.size) {
      if (values(currIdx) > maxVal) {
        maxIdx = currIdx;
        maxVal = values(maxIdx);
      }
      currIdx += 1;
    }
    maxIdx;
  }
  
  def argMaxIdx(values: Seq[Double]) = {
    var currIdx = 0;
    var maxIdx = 0;
    var maxVal = Double.NegativeInfinity;
    while (currIdx < values.size) {
      if (values(currIdx) > maxVal) {
        maxIdx = currIdx;
        maxVal = values(maxIdx);
      }
      currIdx += 1;
    }
    maxIdx;
  }
  
  def scoreIndexedFeats(feats: Seq[Int], weights: Array[Float]): Float = {
    var featIdx = 0;
    var featTotal = 0.0F;
    while (featIdx < feats.size) {
      featTotal += weights(feats(featIdx));
      featIdx += 1;
    }
    featTotal;
  }
  
  def packFeaturesAndWeights(featureIndexer: Indexer[String], weights: Array[Float]): (Indexer[String], Array[Float]) = {
    val newFeatureIndexer = new Indexer[String];
    for (i <- 0 until weights.size) {
      if (weights(i) != 0) {
        newFeatureIndexer.add(featureIndexer.getObject(i));
      }
    }
    val newWeights = new Array[Float](newFeatureIndexer.size());
    Logger.logss("Packing model from " + weights.size + " to " + newWeights.size);
    var newIdx = 0;
    for (i <- 0 until weights.size) {
      if (newFeatureIndexer.contains(featureIndexer.getObject(i))) {
        newWeights(newIdx) = weights(i)
        newIdx += 1;
      }
    }
    require(newIdx == newFeatureIndexer.size);
    (newFeatureIndexer, newWeights);
  }
  
  def packFeaturesAndWeights(featureIndexer: Indexer[String], weights: Array[Double]): (Indexer[String], Array[Double]) = {
    val newFeatureIndexer = new Indexer[String];
    for (i <- 0 until weights.size) {
      if (weights(i) != 0) {
        newFeatureIndexer.add(featureIndexer.getObject(i));
      }
    }
    val newWeights = new Array[Double](newFeatureIndexer.size());
    Logger.logss("Packing model from " + weights.size + " to " + newWeights.size);
    var newIdx = 0;
    for (i <- 0 until weights.size) {
      if (newFeatureIndexer.contains(featureIndexer.getObject(i))) {
        newWeights(newIdx) = weights(i)
        newIdx += 1;
      }
    }
    require(newIdx == newFeatureIndexer.size);
    (newFeatureIndexer, newWeights);
  }
  
  def main(args: Array[String]) {
    println(fmtProb(1.0));
    println(fmtProb(0.01));
    println(fmtProb(0.001));
    println(fmtProb(0.0001));
    println(fmtProb(0.00001));
    println(fmtProb(0.000001));
    println(fmtProb(0.0000001));
    
    println(fmtProb(0.000000000000000000000001));
  }
}
