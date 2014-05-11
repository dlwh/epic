package epic
package preprocess

import scala.collection.mutable.ArrayBuffer
import java.io._
import chalk.corpora.MascSlab
import chalk.slab.Sentence
import breeze.linalg._
import breeze.numerics._
import framework.{ModelObjective, StandardExpectedCounts, Model, Feature}
import nak.data.Example
import breeze.util.{Iterators, Encoder, Index}
import breeze.features.FeatureVector
import breeze.optimize.{L2Regularization, GradientTester}
import java.text.{BreakIterator, StringCharacterIterator, CharacterIterator}
import features._
import chalk.text.analyze.WordShapeGenerator
import epic.parser.features.IndicatorFeature

@SerialVersionUID(1L)
class MLSentenceSegmenter(inf: MLSentenceSegmenter.ClassificationInference) extends chalk.text.segment.SentenceSegmenter with Serializable {

  override def apply(text: String): Iterable[String] = new Iterable[String] {
    def iterator: Iterator[String] = {
      val str: StringCharacterIterator = new StringCharacterIterator(text)
      val iter = MLSentenceSegmenter.potentialSentenceBoundariesIterator(str)
      var lastOffset = 0
      Iterators.fromProducer {
        def rec():Option[String] = {
          if(iter.hasNext) {
            val pos = iter.next()
            println(lastOffset, pos, text.length)
            if(!iter.hasNext || inf.classify(MLSentenceSegmenter.featuresForEndPointDetection(str, pos))) {
              val res = Some(text.substring(lastOffset, math.min(pos + 1, text.length)))
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
      }
    }
  }
}

object MLSentenceSegmenter {

  def loadModel(file: File) = {
    breeze.util.readObject[MLSentenceSegmenter](file)
  }

  def nextPotentialSentenceBoundary(text: CharacterIterator):Int = {
    var start = text.getIndex
    var codepoint = text.currentCodePoint
    while (codepoint != CharacterIterator.DONE) {
      if(isPotentialSentenceBoundary(text, start, codepoint)) {
        text.setIndex(start + 1)
        return start
      }
      start += Character.charCount(codepoint)
      codepoint = text.codePointAt(start)
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
  case class CharTypeFeature(ct: Int, offset: Int = 0) extends Feature
  case class SurroundingCharFeature(prev: String, next: String) extends Feature
  case class SurroundingCharTypeFeature(prev: Int, next: Int) extends Feature
  case class LastWord(w: String, offset: Int = 0) extends Feature
  case class NextWord(w: String, offset: Int = 0) extends Feature
  case class LastWordLength(len: Int, offset: Int = 0) extends Feature
  case object EOFFeature extends Feature
  case object BOFFeature extends Feature
  case object BiasFeature extends Feature
  case class JavaGuess(v: Int) extends Feature

  def featuresForEndPointDetection(text: CharacterIterator, offset: Int, id: String = ""):Array[Feature] = {
    text.setIndex(offset)
    if(text.next() == CharacterIterator.DONE) {
      Array(BiasFeature, EOFFeature)
    } else {
      val buf = new ArrayBuffer[Feature]

      val myFeatures = addCharFeatures(text, offset, 0)

//      for(f <- buf.toIndexedSeq)
//        buf += CrossProductFeature(f, javaAgrees)

//      buf += javaAgrees

      buf ++= addCharFeatures(text, offset, -2)
      buf ++= addCharFeatures(text, offset, -1)
      buf ++= addCharFeatures(text, offset, 1)
      buf ++= addCharFeatures(text, offset, 2)
//      buf += IndicatorFeature(WordShapeGenerator.apply(text.substring(offset - 1, offset + 3)))
      /*
      if(offset > 2) text.stateless {
        text.setIndex(offset - 2)
        val prevSpace = text.previousIndexWhere(!_.isLetterOrDigit)
        val word = LastWord(text.substring(prevSpace + 1, offset - 1))
        buf += word

        for(f <- myFeatures) buf += CrossProductFeature(word, f)
      }
      */
//      buf += LastWordLength(math.log1p(offset - prevSpace).toInt)
      val nextNotSpace = text.nextIndexWhere(c => !c.isSpaceChar && !c.isControl)
      if(nextNotSpace >= 0) {
        text.setIndex(nextNotSpace)
        val nextnextSpace = text.nextIndexWhere(c => c.isSpaceChar || c.isControl)
        if(nextnextSpace >= 0)
          buf += NextWord(text.substring(nextNotSpace, nextnextSpace))
      }
      if(nextNotSpace >= 0)
        buf += LastWord(//Character.getType(text.codePointAt(prevSpace + 1))+
          "--" + Character.getType(text.codePointAt(nextNotSpace + 1)))
      for(f1 <- addCharFeatures(text, offset, 1); f2 <- addCharFeatures(text, offset, 2)) {
        buf += CrossProductFeature(f1, f2)
      }

      for(f1 <- addCharFeatures(text, offset, 0); f2 <- addCharFeatures(text, offset, 1)) {
        buf += CrossProductFeature(f1, f2)
      }

      for(f0 <- addCharFeatures(text, offset, -1); f1 <- addCharFeatures(text, offset, 0); f2 <- addCharFeatures(text, offset, 1)) {
        buf += CrossProductFeature(f0, CrossProductFeature(f1, f2))
      }

      for(f0 <- addCharFeatures(text, offset, 0); f1 <- addCharFeatures(text, offset, 1); f2 <- addCharFeatures(text, offset, 2)) {
        buf += CrossProductFeature(f0, CrossProductFeature(f1, f2))
      }

      for(f1 <- addCharFeatures(text, offset, -1); f2 <- addCharFeatures(text, offset, 0)) {
        buf += CrossProductFeature(f1, f2)
      }

      buf ++= myFeatures

      buf.toArray
    }


  }


  def addCharFeatures(text: CharacterIterator, current: Int, rel: Int): IndexedSeq[Feature] = {
    val buf = new ArrayBuffer[Feature]
    val (cp, cps) = try {
      val ch = text.codePointAt(current + rel)
      (ch.toInt, codepointToString(ch))
    } catch {
      case ex: IllegalArgumentException => 0 -> "###"
    }
    if(Character.getType(cp) == 24)
      buf += new CodePointFeature(cps, rel)
    else buf += new CharTypeFeature(Character.getType(cp), rel)
    text.setIndex(current)
    buf.toIndexedSeq
  }

  // http://www.unicode.org/Public/UCD/latest/ucd/auxiliary/SentenceBreakProperty.txt
  // http://www.unicode.org/reports/tr29/#Sentence_Boundaries
  def isPotentialSentenceBoundary(text: CharacterIterator, offset: Int, codepoint: Int):Boolean = text.stateless {
    text.setIndex(offset)
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
        (
          isControl(codepoint) // a control character is a sentence break
            && offset != 0 //  if it is not the first chracter
            && !isPotentialSentenceBoundary(text, offset - Character.charCount(codepoint), text.codePointBefore(offset)) // the previous char isn't a sentence break
            && ( // one of the following
              text.current() == CharacterIterator.DONE
            || isControl(text.codePointAt(offset + 1)) // two newlines
            || previousLineIsShort(text, offset)
            || nextLineIsShort(text, offset)
            || Character.isUpperCase(text.codePointAt(offset + 1)) // next line starts with upper case
            )
          )
      case Character.OTHER_SYMBOL => false
      case _ => false
    }

  }


  def isControl(codepoint: Int): Boolean = {
    codepoint == '\r' || codepoint == '\n' || codepoint == '\t'
  }


  def previousLineIsShort(s: CharacterIterator, pos: Int): Boolean = s.stateless {
    s.setIndex(pos - 1)
    val SHORT_LINE = 40 // in characters
    (pos - s.previousIndexWhere(_ == '\n') ) < SHORT_LINE
  }

  def nextLineIsShort(s: CharacterIterator, pos: Int): Boolean = s.stateless {
    s.setIndex(pos + 1)
    val SHORT_LINE = 40 // in characters
    (pos - s.nextIndexWhere(_ == '\n') ) < SHORT_LINE
  }


  def isProbablyNotContraction(text: CharacterIterator, offset: Int, codepoint: Int, quote: Char): Boolean = text.stateless {
    (codepoint != quote || codepoint == CharacterIterator.DONE || offset == 0 || !Character.isLetterOrDigit(text.codePointAt(offset + 1)) || !Character.isLetterOrDigit(text.codePointBefore(offset)))
  }

  def potentialSentenceBoundariesIterator(text: CharacterIterator):Iterator[Int] = new Iterator[Int] {
    var offset = 0

    override def hasNext: Boolean = text.current() != CharacterIterator.DONE

    override def next(): Int = {
      offset = nextPotentialSentenceBoundary(text)
      try {
        text.setIndex(offset)
        text.setIndex(offset + 1)
      } catch {
        case ex: IllegalArgumentException =>
      }
      offset
    }

  }

  def adjustGoldSentenceBoundaries(text: CharacterIterator, endPoints: Iterator[Int]):Set[Int] = {
    val mapped = for(_p <- endPoints) yield {
      var p = math.max(_p, 0)
      var cp = text.codePointAt(p)

      if(p > 0 && !Character.isSpaceChar(cp) && !isPotentialSentenceBoundary(text, p, cp)) {
        p -= Character.charCount(cp)
        cp = text.codePointAt(p)
      }

      var earliestControlChar = p
      text.setIndex(p)
      val nextNonSpacePos = text.nextIndexWhere(!_.isSpaceChar)
      if(nextNonSpacePos > p) {
        val ccp = text.codePointAt(nextNonSpacePos)
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

      if(Character.isSpaceChar(cp)) {
        p = earliestControlChar
        cp = text.codePointAt(p)
      }

      p
    }

    mapped.toSet

  }

  case class SentenceDecisionInstance(label: Boolean, features: Array[Feature], id: String) extends Example[Boolean, Array[Feature]]

  def main(args: Array[String]):Unit = {
    val mascDir = new File(args(0))


    val sentenceBoundaryProblems = for(dir <- new File(new File(mascDir,"data"), "written").listFiles()
        if !dir.toString.contains("twitter") && dir.isDirectory;
        f <- dir.listFiles(new FilenameFilter {
      override def accept(dir: File, name: String): Boolean = name.endsWith(".txt")
    })) yield {
      val slab = MascSlab(f.toURI.toURL)
      val slabWithSentences = MascSlab.s(slab)


      val text = slab.content
      val guessPoints: IndexedSeq[Int] = potentialSentenceBoundariesIterator(new ChunkedCharIterator(text.toIndexedSeq.iterator)).toIndexedSeq

      val goldPoints = adjustGoldSentenceBoundaries(new StringCharacterIterator(text), slabWithSentences.iterator[Sentence].map(_.end))

      println("<<<<" + f  )
      printOutSentenceBoundaries(text, guessPoints.toSet, goldPoints)

      for(guess <- guessPoints) yield {
        SentenceDecisionInstance(goldPoints.contains(guess), featuresForEndPointDetection(new StringCharacterIterator(slab.content), guess, s"${f.getName}:$guess"), s"${f.getName}:$guess")
      }
    }

    val (dev, train) = sentenceBoundaryProblems.flatten.splitAt(2400)
    val featureIndex = Index[Feature]()
    for(inst <- train; f <- inst.features) {
      featureIndex.index(f)
    }
    println(train.size)

    val model = new ClassificationModel(featureIndex)

    val obj = new ModelObjective(model, train)
    val bestWeights = breeze.optimize.minimize(obj.cached, obj.initialWeightVector(true), L2Regularization(0.1))

    val inf = model.inferenceFromWeights(bestWeights)

    val decoded = (Encoder.fromIndex(featureIndex).decode(bestWeights))

    println("Train")
    evalDev(inf, train, decoded)
    println("Dev")
    evalDev(inf, dev, decoded)

    breeze.util.writeObject(new File("en-sent-segmenter.model.ser.gz"), new MLSentenceSegmenter(inf))

  }

  def evalDev(inf: ClassificationInference, dev: Array[SentenceDecisionInstance], decoded: Counter[Feature, Double]) {
    var right = 0
    var wrong = 0
    var tN, fN = 0
    var tP, fP = 0
    for (inst <- dev) {
      if (inst.label != inf.classify(inst.features)) {
        println(inst.id, inst.label)
        val weights = (inst.features.toIndexedSeq.map(f => f -> decoded(f)))
        println(weights.sortBy(_._2), weights.map(_._2).sum)
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

    override def accumulateCounts(s: Scorer, d: SentenceDecisionInstance, m: Marginal, accum: ExpectedCounts, scale: Double): Unit = {
      val fs = new FeatureVector(d.features.map(featureIndex).filterNot(_ == -1))
      axpy(m.prob * scale, fs, accum.counts)
      accum.loss += scale * m.logPartition
    }
  }


  @SerialVersionUID(1L)
  case class ClassificationInference(featureIndex: Index[Feature], weights: DenseVector[Double]) extends framework.Inference[SentenceDecisionInstance] {
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
