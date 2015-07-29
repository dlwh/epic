package epic.preprocess

import java.io._
import java.util.zip.GZIPInputStream

import breeze.features.FeatureVector
import breeze.linalg._
import breeze.numerics._
import breeze.optimize.L2Regularization
import breeze.stats.distributions.{RandBasis, Rand}
import breeze.util.{Encoder, Index, Iterators}
import epic.corpora.MascSlab
import epic.features.CrossProductFeature
import epic.framework.{Example, Feature, ModelObjective, StandardExpectedCounts}
import epic.slab.{Sentence, StringSlab}
import epic.trees.Span

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

@SerialVersionUID(1L)
class MLSentenceSegmenter(inf: MLSentenceSegmenter.ClassificationInference) extends SentenceSegmenter with Serializable {
  override def apply[In](slab: StringSlab[In]): StringSlab[In with Sentence] = {
    val text = slab.content
    val iter = MLSentenceSegmenter.potentialSentenceBoundariesIterator(text)
    var lastOffset = 0
    slab.addLayer[Sentence](
      Iterators.fromProducer {
        def rec():Option[(Span, Sentence)] = {
          if(iter.hasNext) {
            val pos = iter.next()
            if(!iter.hasNext || inf.classify(MLSentenceSegmenter.featuresForEndPointDetection(text, pos))) {
              val res = Some(Span(lastOffset, math.min(pos + 1, text.length)) -> Sentence())
              lastOffset = pos + 1
              res
            } else {
              rec()
            }
          } else {
            None
          }
        }
        rec()
      }.filterNot(s => text.substring(s._1.begin, s._1.end).forall(_.isWhitespace))
    )

  }
  override def toString = "MLSentenceSegmenter(...)"
}

object MLSentenceSegmenter {

  def bundled(language: String = "en"):Option[MLSentenceSegmenter] = {
    val path = s"$language-sent-segmenter.model.ser.gz"
    Option(getClass.getResourceAsStream(path)).map { strm =>
      try {
      val oin = new ObjectInputStream(new GZIPInputStream(strm))
      oin.readObject().asInstanceOf[MLSentenceSegmenter]
      } finally {
        if(strm != null)
          strm.close()
      }
    }
  }

  def loadModel(file: File) = {
    breeze.util.readObject[MLSentenceSegmenter](file)
  }

  def nextPotentialSentenceBoundary(text: String, offset: Int):Int = {
    var start = offset + 1
    while (start < text.length) {
      val codepoint = text.codePointAt(start)
      if(isPotentialSentenceBoundary(text, start, codepoint)) {
        return start
      }
      start += Character.charCount(codepoint)
    }
    start
  }

  def codepointToString(cp: Int) = {
    if(Character.charCount(cp) == 1 && !Character.isISOControl(cp) && !Character.isSpaceChar(cp)) {
      cp.toChar.toString
    } else {
      Character.getName(cp)
    }

  }


  case class CodePointFeature(cp: String, offset: Int = 0) extends Feature
  case class NextRealLetterFeature(ct: Int) extends Feature {
    override def toString = {
      s"NextRealLetterFeature(${stringForCharType(ct)}($ct))"
    }
  }
  case class CharTypeFeature(ct: Int, offset: Int = 0) extends Feature {
    override def toString = {
      s"CharTypeFeature(${stringForCharType(ct)}($ct), $offset)"
    }
  }
  case class SurroundingCharFeature(prev: String, next: String) extends Feature
  case class SurroundingCharTypeFeature(prev: Int, next: Int) extends Feature {
    override def toString = {
      s"SurroundingCharTypeFeature(${stringForCharType(prev)}($prev), ${stringForCharType(next)}($next))"
    }

  }
  case class ContextWord(w: String, offset: Int = 0) extends Feature
  case class LastWordLength(len: Int, offset: Int = 0) extends Feature
  case object EOFFeature extends Feature
  case object BOFFeature extends Feature
  case object BiasFeature extends Feature
  case class JavaDistFeature(x: Int) extends Feature
  case object LineIsShortFeature extends Feature

  private def stringForCharType(ct: Int):String = {
    val characterClass = Class.forName("java.lang.Character")
    val fields = characterClass.getDeclaredFields()
    for (f <- fields) {
      try {
        val v = f.getByte(null)
        if (v == ct) {
          return f.getName
        }
      } catch {
        case x: Exception =>
      }
    }
    "???"
  }

  def featuresForEndPointDetection(text: String, offset: Int):Array[Feature] = {
    if (offset == text.length) {
      Array(BiasFeature, EOFFeature)
    } else {
      val buf = new ArrayBuffer[Feature]
//      val break = BreakIterator.getSentenceInstance
//      break.setText(text)
//      val pos = break.following(math.max(offset -  3, 0))
//      buf += JavaDistFeature(math.min(pos - offset, 5))
      buf += BiasFeature
      // baseline features for the current char
      val curCharFeatures: IndexedSeq[Feature] = addCharFeatures(text, offset, 0)
      buf ++= curCharFeatures

      if(previousLineIsShort(text, offset)) {
        buf += LineIsShortFeature
        for(m <- curCharFeatures) {
          buf += CrossProductFeature(LineIsShortFeature, m)
        }
      }

      buf ++= addCharFeatures(text, offset, -2)
      buf ++= addCharFeatures(text, offset, -1)
      buf ++= addCharFeatures(text, offset, 1)
      buf ++= addCharFeatures(text, offset, 2)
      for(f1 <- addCharFeatures(text, offset, 1); f2 <- addCharFeatures(text, offset, 2)) {
        buf += CrossProductFeature(f1, f2)
      }

      for(f1 <- curCharFeatures; f2 <- addCharFeatures(text, offset, 1)) {
        buf += CrossProductFeature(f1, f2)
      }

      for(f1 <- addCharFeatures(text, offset, -1); f2 <- curCharFeatures.take(1)) {
        buf += CrossProductFeature(f1, f2)
      }

      for(f1 <- addCharFeatures(text, offset, -1); f2 <- addCharFeatures(text, offset, 1)) {
        buf += CrossProductFeature(f1, f2)
      }

      for(f1 <- addCharFeatures(text, offset, -1); fmid <- curCharFeatures.take(1); f2 <- addCharFeatures(text, offset, 1)) {
        buf += CrossProductFeature(f1, CrossProductFeature(fmid, f2))
      }


      for(f1 <- addCharFeatures(text, offset, -1); f2 <- addCharFeatures(text, offset, 2)) {
        buf += CrossProductFeature(f1, f2)
      }


      val prevSpace = math.max(text.lastIndexWhere(!_.isLetterOrDigit, offset - 2), -1) // -1 is ok, assume BOS is space
      buf += ContextWord(text.substring(prevSpace + 1, offset))
      buf += LastWordLength(offset - prevSpace)
      val nextNotSpace = text.indexWhere(c => !c.isSpaceChar && !c.isControl, offset + 1)
      if(nextNotSpace >= 0) {
        val nextWordEnd = if (text.charAt(nextNotSpace).isLetterOrDigit){
          text.indexWhere(c => !c.isLetterOrDigit, nextNotSpace + 1)
        } else {
          text.indexWhere(c => Character.getType(c) != text.charAt(nextNotSpace), nextNotSpace + 1)
        }
        buf += ContextWord(text.substring(prevSpace + 1, prevSpace + 2)+"--" + text.substring(nextNotSpace, nextNotSpace + 1), -3)
//        if(nextWordEnd >= 0) {
//          buf += ContextWord(text.substring(nextNotSpace, nextWordEnd), 1)
//        }
      }

      val nextLetterPos = text.indexWhere(_.isLetterOrDigit, offset + 1)
      if(nextLetterPos >= 0) {
        buf += NextRealLetterFeature(Character.getType(text.charAt(nextLetterPos)))
      }


      buf += SurroundingCharFeature(if (offset == 0) "BOS" else codepointToString(text.codePointBefore(offset)),
                                    if (nextNotSpace < 0) "EOS" else codepointToString(text.codePointAt(nextNotSpace)))

      buf += SurroundingCharTypeFeature(if (offset == 0) -1 else Character.getType(text.codePointBefore(offset)),
        if (nextNotSpace < 0) -1 else Character.getType(text.codePointAt(nextNotSpace)))

      buf += CrossProductFeature(buf(1), buf.last)

      buf.toArray
    }


  }


  def addCharFeatures(text: String, base: Int, rel: Int): IndexedSeq[Feature] = {
    val buf = new ArrayBuffer[Feature]
    val next = try {text.offsetByCodePoints(base, rel)} catch { case ex: IndexOutOfBoundsException => if(rel > 0) text.length else 0}
    val (cp, cps) =
    if(next < 0 || next >= text.length) {
      0 -> "###"
    } else {
      val cp = text.codePointAt(next)
      val cps = codepointToString(cp)
      cp -> cps
    }
    buf += new CharTypeFeature(Character.getType(cp), rel)
    buf += new CodePointFeature(cps, rel)
    buf.toIndexedSeq
  }

  def isQuote(ch: Char) = {
    Character.getType(ch) match {
      case Character.INITIAL_QUOTE_PUNCTUATION => true
      case Character.FINAL_QUOTE_PUNCTUATION => true
      case Character.OTHER_PUNCTUATION if ch == '\'' || ch == '"' => true
      case _ => false
    }

  }

  // http://www.unicode.org/Public/UCD/latest/ucd/auxiliary/SentenceBreakProperty.txt
  // http://www.unicode.org/reports/tr29/#Sentence_Boundaries
  def isPotentialSentenceBoundary(text: String, offset: Int, codepoint: Int):Boolean = {
    Character.getType(codepoint) match {
      case Character.OTHER_PUNCTUATION => codepoint != ',' && isProbablyNotContraction(text, offset, codepoint, '\'')
      case Character.INITIAL_QUOTE_PUNCTUATION => true
      case Character.START_PUNCTUATION => true
      case Character.FINAL_QUOTE_PUNCTUATION => isProbablyNotContraction(text, offset, codepoint, '’')
      case Character.END_PUNCTUATION => true
      case Character.SPACE_SEPARATOR =>
        offset > 0 && {
          val before = text.codePointBefore(offset)
          !Character.isLetterOrDigit(before) &&
          !Character.isSpaceChar(before) &&
          !isControl(before) &&
          !isPotentialSentenceBoundary(text, offset - Character.charCount(before), before) &&
          before != ','
        }
      case Character.CONTROL =>
         isControl(codepoint) && (offset == 0 ||
//          !isPotentialSentenceBoundary(text, offset - Character.charCount(codepoint), text.codePointBefore(offset))
             text.codePointBefore(offset)!= ','
           && (offset == text.length - 1 || isControl(text.codePointAt(offset + 1)) || previousLineIsShort(text, offset) || Character.isUpperCase(text.codePointAt(offset + 1)))
          )
      case Character.OTHER_SYMBOL => false
      case _ => false
    }

  }


  def isControl(codepoint: Int): Boolean = {
    codepoint == '\r' || codepoint == '\n' || codepoint == '\t'
  }


  def previousLineIsShort(s: String, pos: Int): Boolean = {
    val SHORT_LINE = 35 // in characters
    (pos - s.lastIndexOf('\n', pos - 1) ) < SHORT_LINE
  }


  def isProbablyNotContraction(text: String, offset: Int, codepoint: Int, quote: Char): Boolean = {
    (codepoint != quote || offset >= text.length - 1 || offset == 0 || !Character.isLetterOrDigit(text.codePointAt(offset + 1)) || !Character.isLetterOrDigit(text.codePointBefore(offset)))
  }

  def potentialSentenceBoundariesIterator(text: String):Iterator[Int] = new Iterator[Int] {
    var offset = 0

    override def hasNext: Boolean = offset < text.length

    override def next(): Int = {
      offset = nextPotentialSentenceBoundary(text, offset)
      offset
    }

  }

  def adjustGoldSentenceBoundaries(text: String, _endPoints: Iterator[Span]):Set[Int] = {
    // sort so that longer spans come first, to deal with nesting. (We don't want nested sentences)
    val endPoints = _endPoints.toIndexedSeq.sortBy(s => (s.begin, -s.length))
    var lastSpan = Span(0, 0)
    val mapped = for(s@Span(begin, _p) <- endPoints if !lastSpan.crosses(s) && !lastSpan.contains(s)) yield {
      var p = math.max(_p, 0)

      var cp = text.codePointAt(p)

      if(p > 0 && !Character.isSpaceChar(cp) && !isPotentialSentenceBoundary(text, p, cp)) {
        p -= Character.charCount(cp)
        cp = text.codePointAt(p)
      }

      var earliestControlChar = p
      val nextNonSpacePos = text.indexWhere(!_.isSpaceChar, p)
      if(nextNonSpacePos > p) {
        val ccp = text.charAt(nextNonSpacePos)
        if (ccp == '\n' || ccp == '\t' || ccp == '\r') {
          earliestControlChar = nextNonSpacePos
        }
      }

      while(p > 0 && (Character.isSpaceChar(cp) || cp == '\n' || cp == '\t' || cp == '\r')) {
        if(!Character.isSpaceChar(cp)) {
          earliestControlChar = p
        }
        p -= Character.charCount(cp)
        cp = text.codePointAt(p)
      }


      if(!isPotentialSentenceBoundary(text, p, cp)) {
        p += Character.charCount(cp)
        cp = text.codePointAt(p)
      }

      if(Character.isSpaceChar(cp) && p < text.length) {
        p = earliestControlChar
        cp = text.codePointAt(p)
      }

      if(lastSpan.crosses(s) || lastSpan.contains(s)) {
        println(text.substring(lastSpan.begin, lastSpan.end))
        println(text.substring(s.begin, s.end))
        println(text.charAt(p))
        println(text.charAt(s.end))
        println(text.charAt(lastSpan.end))
        println("====")
      }
      lastSpan = s

      p
    }

    mapped.toSet

  }

  case class SentenceDecisionInstance(label: Boolean,
                                      features: Array[Feature],
                                      id: String,
                                      context: String) extends Example[Boolean, Array[Feature]]

  def main(args: Array[String]):Unit = {
    val mascDir = new File(args(0))


    val sentenceBoundaryProblems = for(dir <- new File(new File(mascDir,"data"), "written").listFiles()
        if !dir.toString.contains("twitter") && dir.isDirectory;
        f <- dir.listFiles(new FilenameFilter {
      override def accept(dir: File, name: String): Boolean = name.endsWith(".txt")
    })) yield {
      val slab = MascSlab(f.toURI.toURL)
      val slabWithSentences = MascSlab.s(slab)


      val guessPoints: IndexedSeq[Int] = potentialSentenceBoundariesIterator(slabWithSentences.content).toIndexedSeq

      val text = slab.content
      val goldPoints = adjustGoldSentenceBoundaries(text, slabWithSentences.iterator[Sentence].map(_._1))

//      println("<<<<" + f  )
//      printOutSentenceBoundaries(text, guessPoints.toSet, goldPoints)

      for(guess <- guessPoints) yield {
        val contextBegin = math.max(0, guess - 50)
        val contextEnd = math.min(text.length, guess + 50)
        val context =   if(guess != text.length) {
          text.substring(contextBegin, guess) + "[[" + text.charAt(guess) + "]]" +  text.substring(guess + 1, contextEnd)
        } else {
          text.substring(contextBegin, guess) + "[[]]"
        }

        SentenceDecisionInstance(goldPoints.contains(guess),
          featuresForEndPointDetection(slab.content, guess),
          s"${f.getName}:$guess", context)
      }
    }


    val allProbs = sentenceBoundaryProblems.flatten
    val perm = RandBasis.mt0.permutation(allProbs.length).draw()
    val (dev, train) = perm.map(allProbs).splitAt(1000)
    val featureIndex = Index[Feature]()
    for(inst <- train; f <- inst.features) {
      featureIndex.index(f)
    }
    println(train.size)

    val model = new ClassificationModel(featureIndex)

    val obj = new ModelObjective(model, train)
    val bestWeights = breeze.optimize.minimize(obj.cached, obj.initialWeightVector(true), L2Regularization(1.0))

    val inf = model.inferenceFromWeights(bestWeights)

    val decoded = (Encoder.fromIndex(featureIndex).decode(bestWeights))

    println("Train")
    evalDev(inf, train, decoded)
    println("Dev")
    evalDev(inf, dev, decoded)


    val segmenter: MLSentenceSegmenter = new MLSentenceSegmenter(inf)
    breeze.util.writeObject(new File("en-sent-segmenter.model.ser.gz"), segmenter)

  }

  def evalDev(inf: ClassificationInference, dev: IndexedSeq[SentenceDecisionInstance], decoded: Counter[Feature, Double]) {
    var right = 0
    var wrong = 0
    var tN, fN = 0
    var tP, fP = 0
    for(inst <- dev) {
      if(inst.label != inf.classify(inst.features)) {
        val weights = (inst.features.toIndexedSeq.map( f => f -> decoded(f)))
        val sum: Double = weights.map(_._2).sum
        println(inst.label, inst.id, sum)
        println(inst.context)
        println(weights.sortBy(_._2), sum)
        wrong += 1
        if (inst.label) {
          fN += 1
        } else {
          fP += 1
        }
      } else {
        if (inst.label) {
          tP += 1
        } else {
          tN += 1
        }
        right += 1
      }
    }

    println(s"prec: ${tP * 1.0 / (tP + fP)} rec: ${tP * 1.0 / (tP + fN)}, $tP $tN $fP $fN")

    println(s"$right $wrong... ${right * 1.0 / (right + wrong)}")
  }

  def printOutSentenceBoundaries(text: String, guessPoints: Set[Int], goldPoints: Set[Int]): Unit = {
    for (pos <- 0 until text.length) {
      if (guessPoints(pos) && goldPoints(pos)) print("[=")
      else if (goldPoints(pos)) print("[[")
      else if (guessPoints(pos)) print("{{")
      print(text.charAt(pos))
      if (guessPoints(pos) && goldPoints(pos)) print("=]")
      else if (goldPoints(pos)) print("]]")
      else if (guessPoints(pos)) print("}}")
    }
  }


  case class Marginal(prob: Double, logPartition: Double) extends epic.framework.Marginal

  class ClassificationModel(val featureIndex: Index[Feature]) extends StandardExpectedCounts.Model[SentenceDecisionInstance] {
    override def initialValueForFeature(f: Feature): Double = 0.0

    type Marginal = MLSentenceSegmenter.Marginal
    type Inference = MLSentenceSegmenter.ClassificationInference
    type Scorer = ClassificationInference


    override def inferenceFromWeights(weights: DenseVector[Double]): Inference = new ClassificationInference(featureIndex, weights)

    override def accumulateCounts(inf: Inference, s: Scorer, d: SentenceDecisionInstance, m: Marginal, accum: ExpectedCounts, scale: Double): Unit = {
      val fs = new FeatureVector(d.features.map(featureIndex).filterNot(_ == -1))
      axpy(m.prob * scale, fs, accum.counts)
      accum.loss += scale * m.logPartition
    }
  }


  @SerialVersionUID(1L)
  case class ClassificationInference(featureIndex: Index[Feature], weights: DenseVector[Double]) extends epic.framework.Inference[SentenceDecisionInstance] {
    type Scorer = ClassificationInference
    override def scorer(v: SentenceDecisionInstance): Scorer = this

    def classify(features: Array[Feature]): Boolean = {
      val fs = new FeatureVector(features.map(featureIndex).filterNot(_ == -1))
      val act = weights dot fs
      val prob = sigmoid(act)
      prob > 0.5
    }

    override type Marginal = MLSentenceSegmenter.Marginal

    override def goldMarginal(scorer: Scorer, v: SentenceDecisionInstance): Marginal = {
      val act = if (v.label) {
        val fs = new FeatureVector(v.features.map(featureIndex).filterNot(_ == -1))
        weights dot fs
      } else {
        0.0
      }
      Marginal(I(v.label), act)
    }

    override def marginal(scorer: Scorer, v: SentenceDecisionInstance): Marginal = {
      val fs = new FeatureVector(v.features.map(featureIndex).filterNot(_ == -1))
      val act = weights dot fs
      val prob = sigmoid(act)

      Marginal(prob, -log1p(-prob))
    }
  }
}
