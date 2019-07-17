package epic.sentiment

object SentimentEvaluator {
  
  def socherDevSpanMatrix = readStringMatrix("""
       135       762       130        42         1
        57      2817      1465       267         7
         8      1133     25941      1213        10
         1       392      1340      3926       122
         2        84       106       914       572
""")
  
  def socherDevRootMatrix = readStringMatrix("""
        14       112         7         6         0
        17       217        24        30         1
         2       112        41        72         2
         1        68        14       165        31
         1        17         1       100        46
""")
  
  def socherNonneutralDevSpanMatrix = readStringMatrix("""
       119       690       114        35         1
        54      2318      1166       194         6
         6       745     20576       855         5
         1       314      1088      3306       113
         2        78        97       828       509
""")
  
  def socherNonneutralDevRootMatrix = readStringMatrix("""
        14       112         7         6         0
        17       217        24        30         1
         0         0         0         0         0
         1        68        14       165        31
         1        17         1       100        46
""")
  
  def socherNonneutralTestSpanMatrix = readStringMatrix("""
       295      1166       250        89         0
       147      4641      2466       497         0
        11      1592     42470      1694         7
         3       538      2404      6205       216
         3       142       253      2013      1123
""")
  
  def socherNonneutralTestRootMatrix = readStringMatrix("""
        44       193        23        19         0
        39       451        62        81         0
         0         0         0         0         0
         0       131        31       297        51
         0        36         8       255       100
""")
  
  def socherNonneutralTestSpanMatrixNew = transpose(readStringMatrix("""
        294     147      11       3       3   
        1167    4639    1587     537     141  
        250    2467   42469    2403     252   
        89     498    1700    6208    2014   
        0       0       7     215    1124    
"""))
  
  def socherNonneutralTestRootMatrixNew = transpose(readStringMatrix("""
        44      39       0       0       0    
        193     451       0     131      36   
        23      62       0      30       8    
        19      81       0     299     255    
        0       0       0      50     100    
"""))

  def readStringMatrix(str: String) = str.split("\n").map(_.trim).filter(!_.isEmpty).map(_.split("\\s+").map(_.toInt))
  def transpose(arr: Array[Array[Int]]) = arr.transpose
  
  def printFromConfusionMatrix(mat: Array[Array[Int]]) {
//    println("Accuracy: " + accuracy(mat)); // agrees with the Stanford system's way of combining the matrix
//    println("Ternary: " + ternaryCoarseEval(mat))
//    println("Binary: " + binaryCoarseEval(mat))
//    println("Socher binary: " + socherCoarseEval(mat)); // agrees with the Stanford system's way of combining the matrix
    println("Accuracy: " + accuracy(mat, isCorrectNormal, isUsedAlways)); // agrees with the Stanford system's way of combining the matrix
    println("Ternary: " + accuracy(mat, isCorrectTernary, isUsedAlways))
    println("Binary: " + accuracy(mat, isCorrectBinary, isUsedBinaryCoarse))
  }
  
  def accuracy(mat: Array[Array[Int]]) = {
    val numer = mat.indices.map(i => mat(i)(i)).reduce(_+_)
    val denom = mat.indices.map(i => mat(i).reduce(_+_)).reduce(_+_)
    renderNumerDenom(numer, denom)
  }
  
  def accuracy(mat: Array[Array[Int]], isCorrect: (Int, Int) => Boolean, isUsed: (Int, Int) => Boolean) = {
    val numer = mat.indices.map(i => mat(i).indices.map(j => {
      if (isUsed(i, j) && isCorrect(i, j)) mat(i)(j) else 0
    }).reduce(_+_)).reduce(_+_)
    val denom = mat.indices.map(i => mat(i).indices.map(j => {
      if (isUsed(i, j)) mat(i)(j) else 0
    }).reduce(_+_)).reduce(_+_)
    renderNumerDenom(numer, denom)
  }
  
  def isCorrectNormal(gold: Int, guess: Int) = gold == guess
  def isCorrectTernary(gold: Int, guess: Int) = (gold < 2 && guess < 2) || (gold > 2 && guess > 2) || (gold == 2 && guess == 2)
  def isCorrectBinary(gold: Int, guess: Int) = (gold < 2 && guess < 2) || (gold > 2 && guess > 2)
  
  def isUsedAlways(gold: Int, guess: Int) = true
  def isUsedBinaryCoarse(gold: Int, guess: Int) = gold != 2
  
//  def ternaryCoarseEval(mat: Array[Array[Int]]) = {
//    val numer = mat(0)(0) + mat(0)(1) + mat(1)(0) + mat(1)(1) + mat(2)(2) + mat(3)(3) + mat(3)(4) + mat(4)(3) + mat(4)(4); 
//    val denom = (0 until mat.size).map(i => mat(i).reduce(_+_)).reduce(_+_)
//    renderNumerDenom(numer, denom)
//  }
//  
//  def binaryCoarseEval(mat: Array[Array[Int]]) = {
//    val numer = mat(0)(0) + mat(0)(1) + mat(1)(0) + mat(1)(1) + mat(3)(3) + mat(3)(4) + mat(4)(3) + mat(4)(4)
//    val denom = numer + mat(0)(3) + mat(0)(4) + mat(1)(3) + mat(1)(4) + mat(3)(0) + mat(3)(1) + mat(4)(0) + mat(4)(1) + mat(0)(2) + mat(1)(2) + mat(3)(2) + mat(4)(2)
//    renderNumerDenom(numer, denom)
//  }
//  
//  def socherCoarseEval(mat: Array[Array[Int]]) = {
//    val numer = mat(0)(0) + mat(0)(1) + mat(1)(0) + mat(1)(1) + mat(3)(3) + mat(3)(4) + mat(4)(3) + mat(4)(4)
//    val denom = numer + mat(0)(3) + mat(0)(4) + mat(1)(3) + mat(1)(4) + mat(3)(0) + mat(3)(1) + mat(4)(0) + mat(4)(1)
//    renderNumerDenom(numer, denom)
//  }
  
  def renderNumerDenom(numer: Int, denom: Int) = {
    numer + " / " + denom + " = " + (numer.toDouble/denom.toDouble)
  }
  
  def main(args: Array[String]) {
    println("DEV SPAN")
    printFromConfusionMatrix(socherDevSpanMatrix)
    println("DEV ROOT")
    printFromConfusionMatrix(socherDevRootMatrix)
    println("NONNEUTRAL DEV SPAN")
    printFromConfusionMatrix(socherNonneutralDevSpanMatrix)
    println("NONNEUTRAL DEV ROOT")
    printFromConfusionMatrix(socherNonneutralDevRootMatrix)
    println("NONNEUTRAL TEST SPAN")
    printFromConfusionMatrix(socherNonneutralTestSpanMatrix)
    println("NONNEUTRAL TEST ROOT")
    printFromConfusionMatrix(socherNonneutralTestRootMatrix)
    println("NONNEUTRAL TEST SPAN NEW")
    printFromConfusionMatrix(socherNonneutralTestSpanMatrixNew)
    println("NONNEUTRAL TEST ROOT NEW")
    printFromConfusionMatrix(socherNonneutralTestRootMatrixNew)
  }
}