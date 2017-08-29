package epic.preprocess

import java.io._
import java.util.zip.GZIPInputStream

import breeze.features.FeatureVector
import breeze.linalg._
import breeze.numerics._
import breeze.optimize.L2Regularization
import breeze.stats.distributions.{ Rand, RandBasis }
import breeze.util.{ Encoder, Index, Iterators }
import epic.corpora.MascSlab
import epic.features.CrossProductFeature
import epic.framework.{ Example, Feature, ModelObjective, StandardExpectedCounts }
import epic.slab.{ Sentence, StringSlab }
import epic.trees.Span

import scala.collection.SortedSet
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

@SerialVersionUID(1L)
case class MLSentenceSegmenter(inf: MLSentenceSegmenter.ClassificationInference) extends SentenceSegmenter with Serializable {
  override def apply[In](slab: StringSlab[In]): StringSlab[In with Sentence] = {
    val text = slab.content
    val iter = MLSentenceSegmenter.potentialSentenceBoundariesIterator(text)
    var lastOffset = 0
    slab.addLayer[Sentence](
      Iterators.fromProducer {
        def rec():Option[(Span, Sentence)] = {
          if (iter.hasNext) {
            val pos = iter.next()
            if (!iter.hasNext || inf.classify(MLSentenceSegmenter.featuresForEndPointDetection(text, pos))) {
              val res = Some(Span(lastOffset, math.min(pos + 1, text.length)) -> Sentence())
              lastOffset = pos + 1
              res
            } else {
              rec()
            }
          } else if (lastOffset == 0) {
            lastOffset = text.length + 1
            Some(Span(0, text.length) -> Sentence())
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
        if (strm != null)
          strm.close()
      }
    }
  }

  def loadModel(file: File) = {
    breeze.util.readObject[MLSentenceSegmenter](file)
  }

  def nextPotentialSentenceBoundary(text: String, offset: Int): Int = {
    var start = offset + 1
    while (start < text.length) {
      val codepoint = text.codePointAt(start)
      if (isPotentialSentenceBoundary(text, start, codepoint)) {
        return start
      }
      start += Character.charCount(codepoint)
    }
    start
  }

  def codepointToString(cp: Int) = {
    if (Character.charCount(cp) == 1 && !Character.isISOControl(cp) && !Character.isSpaceChar(cp)) {
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
  @SerialVersionUID(-2080712838462299209L)
  case object EOFFeature extends Feature
  case object BOFFeature extends Feature
  @SerialVersionUID(8117974141443304644L)
  case object BiasFeature extends Feature
  case class JavaDistFeature(x: Int) extends Feature
  @SerialVersionUID(4081948270063241907L)
  case object LineIsShortFeature extends Feature

  private def stringForCharType(ct: Int): String = {
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
    val buf = new ArrayBuffer[Feature]
    buf += BiasFeature
    // val break = BreakIterator.getSentenceInstance
    // break.setText(text)
    // val pos = break.following(math.max(offset -  3, 0))
    // buf += JavaDistFeature(math.min(pos - offset, 5))
    if (offset == text.length) {
      buf += EOFFeature
    }
    // baseline features for the current char
    val curCharFeatures: IndexedSeq[Feature] = addCharFeatures(text, offset, 0)
    buf ++= curCharFeatures

    if (previousLineIsShort(text, offset)) {
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
    if (nextNotSpace >= 0) {
      val nextWordEnd = if (text.charAt(nextNotSpace).isLetterOrDigit){
        text.indexWhere(c => !c.isLetterOrDigit, nextNotSpace + 1)
      } else {
        text.indexWhere(c => Character.getType(c) != text.charAt(nextNotSpace), nextNotSpace + 1)
      }
      buf += ContextWord(text.substring(prevSpace + 1, prevSpace + 2)+"--" + text.substring(nextNotSpace, nextNotSpace + 1), -3)
      // if (nextWordEnd >= 0) {
      //   buf += ContextWord(text.substring(nextNotSpace, nextWordEnd), 1)
      // }
    }

    val nextLetterPos = text.indexWhere(_.isLetterOrDigit, offset + 1)
    if (nextLetterPos >= 0) {
      buf += NextRealLetterFeature(Character.getType(text.charAt(nextLetterPos)))
    }

    buf += SurroundingCharFeature(if (offset == 0) "BOS" else codepointToString(text.codePointBefore(offset)),
      if (nextNotSpace < 0) "EOS" else codepointToString(text.codePointAt(nextNotSpace)))

    buf += SurroundingCharTypeFeature(if (offset == 0) -1 else Character.getType(text.codePointBefore(offset)),
      if (nextNotSpace < 0) -1 else Character.getType(text.codePointAt(nextNotSpace)))

    buf += CrossProductFeature(buf(1), buf.last)

    buf.toArray
  }

  def addCharFeatures(text: String, base: Int, rel: Int): IndexedSeq[Feature] = {
    val buf = new ArrayBuffer[Feature]
    val next = try {text.offsetByCodePoints(base, rel)} catch { case ex: IndexOutOfBoundsException => if (rel > 0) text.length else 0}
    val (cp, cps) =
    if (next < 0 || next >= text.length) {
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
  def isPotentialSentenceBoundary(text: String, offset: Int, codepoint: Int): Boolean = {
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
          // !isPotentialSentenceBoundary(text, offset - Character.charCount(codepoint), text.codePointBefore(offset))
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
    codepoint != quote || offset >= text.length - 1 || offset == 0 || !Character.isLetterOrDigit(text.codePointAt(offset + 1)) || !Character.isLetterOrDigit(text.codePointBefore(offset))
  }

  def potentialSentenceBoundariesIterator(text: String):Iterator[Int] = {
    var offset = 0
    Iterators.fromProducer {
      if (offset == text.length && text.nonEmpty) {
        None
      } else {
        val next = nextPotentialSentenceBoundary(text, offset)
        // don't generate EOF boundary if previous character was possibly a sentence boundary
        if (next < text.length || next != offset + 1) {
          offset = next
          Some(next)
        } else {
          None
        }
      }


    }
  }

  def adjustGoldSentenceBoundaries(text: String, _endPoints: Iterator[Span], newlineIsBoundary: Boolean):Set[Int] = {
    // sort so that longer spans are last, to deal with nesting.
    val endPoints = _endPoints.toIndexedSeq.sortBy(s => (s.begin, s.length))
    var lastSpan = Span(0, 0)
    val mapped = for(s@Span(begin, _p) <- endPoints if !lastSpan.crosses(s) && !lastSpan.contains(s) && !s.contains(lastSpan)) yield {
      var p = math.max(_p, 0)
      var cp = text.codePointAt(p)

      if (p > 0 && !Character.isSpaceChar(cp) && !isPotentialSentenceBoundary(text, p, cp)) {
        p -= Character.charCount(cp)
        cp = text.codePointAt(p)
      }

      var earliestControlChar = p
      val nextNonSpacePos = text.indexWhere(!_.isSpaceChar, p)
      if (nextNonSpacePos > p) {
        val ccp = text.charAt(nextNonSpacePos)
        if (isControlChar(ccp)) {
          earliestControlChar = nextNonSpacePos
        }
      }

      // rewind to the first possible newline or space we can use.
      while (p > 0 && (Character.isSpaceChar(cp) || isControl(cp))) {
        if (!Character.isSpaceChar(cp)) {
          earliestControlChar = p
        }
        p -= Character.charCount(cp)
        cp = text.codePointAt(p)
      }

      if (!isPotentialSentenceBoundary(text, p, cp)) {
        p += Character.charCount(cp)
        cp = text.codePointAt(p)
      }

      if (Character.isSpaceChar(cp) && p < text.length) {
        p = earliestControlChar
        cp = text.codePointAt(p)
      }

      if (lastSpan.crosses(s) || lastSpan.contains(s)) {
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

    val annotated = mapped.toSet
    var allBoundaries = annotated

    if (newlineIsBoundary) {
      val codepoints = text.codePoints().toArray.zipWithIndex
      for ((cp, pos) <- codepoints) {
        if (cp == '\n' && !annotated(pos)) {
          // if there's a sentence boundary somewhere in the span of spaces containing this
          // newline, don't add this as a boundary
          val previousBoundary = codepoints.lastIndexWhere({
              case (otherCp, otherPos) => (!Character.isSpaceChar(otherCp) && !isControl(otherCp)) || allBoundaries(otherPos)
          }, pos)
          val nextBoundary = codepoints.indexWhere({
            case (otherCp, otherPos) => (!Character.isSpaceChar(otherCp) && !isControl(otherCp)) || allBoundaries(otherPos)
          }, pos)
          if (previousBoundary != -1 && !allBoundaries(previousBoundary) && nextBoundary != -1 && !allBoundaries(nextBoundary)) {
            allBoundaries += pos
          }
        }
      }
    }

    if (newlineIsBoundary) {
      println((allBoundaries -- annotated).toIndexedSeq.sorted)
    }

    allBoundaries

  }

  private def isControlChar(ccp: Char): Boolean = {
    ccp == '\n' || ccp == '\t' || ccp == '\r'
  }

  case class SentenceDecisionInstance(label: Boolean,
                                      features: Array[Feature],
                                      id: String,
                                      context: String) extends Example[Boolean, Array[Feature]]

  implicit class addIndicesWhere(private val __str: String) extends AnyVal {
    def indicesWhere(f: Char=>Boolean) = (0 until __str.length).filter(i => f(__str.charAt(i)))

  }

  def instancesAtPeriods(str: String) = str -> str.indicesWhere(_ == '.')

  val extraExamples = IndexedSeq (
    instancesAtPeriods("Hello to you.Let's go outside."),
    instancesAtPeriods("Among the items up for sale: the Singer sewing machine the embattled domestic diva used to sew her own wedding dress back in 1961. He also hocked the double boiler."),
    instancesAtPeriods("too expensive. do you have anything cheaper."),
    instancesAtPeriods("i'm planning a trip to san diego. flying from chicago on 1/3 returning 1/7.")
  )

  def main(args: Array[String]):Unit = {
    val mascDir = new File(args(0))

    var sentenceBoundaryProblems = for(dir <- new File(new File(mascDir,"data"), "written").listFiles()
        if dir.isDirectory;
        f <- dir.listFiles(new FilenameFilter {
      override def accept(dir: File, name: String): Boolean = name.endsWith(".txt")
    })) yield {
      val slab = MascSlab(f.toURI.toURL)
      val slabWithSentences = MascSlab.s(slab)
      val text = slab.content

      val guessPoints: IndexedSeq[Int] = potentialSentenceBoundariesIterator(slabWithSentences.content).toIndexedSeq

      // twitter data is useful because of lots of caseless stuff, but they have one
      // tweet per line. line endings don't usually get annotated as sentence boundaries,
      // and the context from previous/next lines isn't actually real, so we need to fix that.
      val newlineIsDocBoundary = dir.getName == "twitter"

      val goldPoints: Set[Int] = adjustGoldSentenceBoundaries(text, slabWithSentences.iterator[Sentence].map(_._1), newlineIsDocBoundary)

      for(guess <- guessPoints) yield {
        val (textForFeatures, guessForFeatures) = {
          if (newlineIsDocBoundary && guess > 0) {
            var mostRecentNewline = text.lastIndexWhere(_ == '\n', guess- 1)
            if (mostRecentNewline < 0) {
              mostRecentNewline = 0
            }
            var nextNewline = text.indexWhere(_ == '\n', guess)
            if (nextNewline < 0) {
              nextNewline = text.length
            }

            (text.substring(mostRecentNewline, nextNewline), guess - mostRecentNewline)
          } else {
            (text, guess)
          }
        }

        val contextForFeatures: String = makeStringContext(textForFeatures, guessForFeatures)

        val features = featuresForEndPointDetection(textForFeatures, guessForFeatures)
        SentenceDecisionInstance(goldPoints.contains(guess),
          features,
          s"${f.getName}:$guess", contextForFeatures)
      }
    }

    val extraInstances = {
      for ( (text, goldPoints) <- extraExamples) yield {
        val guessPoints: IndexedSeq[Int] = potentialSentenceBoundariesIterator(text).toIndexedSeq
        for (guess <- guessPoints) yield {
          val context: String = makeStringContext(text, guess)

          val features = featuresForEndPointDetection(text, guess)
          SentenceDecisionInstance(goldPoints.contains(guess),
            features,
            s"$text:$guess", context)

        }

      }
    }

    sentenceBoundaryProblems ++= extraInstances

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

    val decoded = Encoder.fromIndex(featureIndex).decode(bestWeights)

    println("Train")
    evalDev(inf, train, decoded)
    println("Dev")
    evalDev(inf, dev, decoded)
    println("Special")
    evalDev(inf, extraInstances.flatten, decoded)

    val segmenter: MLSentenceSegmenter = new MLSentenceSegmenter(inf)
    breeze.util.writeObject(new File("en-sent-segmenter.model.ser.gz"), segmenter)

  }

  private def makeStringContext(text: String, position: Int): String = {
    val contextBegin = math.max(0, position - 50)
    val contextEnd = math.min(text.length, position + 50)
    if (position != text.length) {
      text.substring(contextBegin, position) + "[[" + text.charAt(position) + "]]" + text.substring(position + 1, contextEnd)
    } else {
      text.substring(contextBegin, position) + "[[]]"
    }
  }

  def evalDev(inf: ClassificationInference, dev: IndexedSeq[SentenceDecisionInstance], decoded: Counter[Feature, Double]) {
    var right = 0
    var wrong = 0
    var tN, fN = 0
    var tP, fP = 0
    for(inst <- dev) {
      if (inst.label != inf.classify(inst.features)) {
        val weights = inst.features.toIndexedSeq.map(f => f -> decoded(f))
        val sum: Double = weights.map(_._2).sum
        println("===========")
        println(s"label: ${inst.label} id: ${inst.id} score: ${sum}")
        println(inst.context)
        println(weights.sortBy(-_._2.abs).scanLeft(null: Any, 0.0, 0.0){ (acc, z) => (z._1, z._2, z._2 + acc._3)}.drop(1), sum)
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

    println(s"Accuracy: $right $wrong... ${right * 1.0 / (right + wrong)}")
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
