package epic.preprocess

import scala.collection.mutable.ArrayBuffer
import java.io._
import chalk.corpora.MascSlab
import chalk.slab.Sentence
import scala.collection.GenMap
import scala.annotation.tailrec
import java.text.BreakIterator
import breeze.linalg.Counter

class MLSentenceSegmenter extends chalk.text.segment.SentenceSegmenter {



  override def apply(text: String): Iterable[String] = {
    val sentences = ArrayBuffer[String]()
    val length = text.length
    var lastSentenceBreakOffset = 0
    var offset = 0
    while(offset < length) {
      val codepoint = text.codePointAt(offset)

      offset += Character.charCount(codepoint)
    }

    sentences
  }
}

object MLSentenceSegmenter {

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
          !isPotentialSentenceBoundary(text, offset - Character.charCount(codepoint), text.codePointBefore(offset))
            && text.codePointBefore(offset)!= ','
           && (offset == text.length - 1 || isControl(text.codePointAt(offset + 1)) || previousLineIsShort(text, offset))
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
    (codepoint != quote || offset == text.length || offset == 0 || !Character.isLetterOrDigit(text.codePointAt(offset + 1)) || !Character.isLetterOrDigit(text.codePointBefore(offset)))
  }

  def potentialSentenceBoundariesIterator(text: String):Iterator[Int] = new Iterator[Int] {
    var offset = 0

    override def hasNext: Boolean = offset < text.length

    override def next(): Int = {
      offset = nextPotentialSentenceBoundary(text, offset)
      offset
    }

  }

  def adjustEndPoints(text: String, endPoints: Iterator[Int]):Set[Int] = {
    val mapped = for(_p <- endPoints) yield {
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

      p
    }

    mapped.toSet

  }

  def main(args: Array[String]):Unit = {
    val mascDir = new File(args(0))
    var numRight, total, totalGuess = 0

    val goldCounts = Counter[Char, Int]()
    val guessCounts = Counter[Char, Int]()

    for(dir <- new File(new File(mascDir,"data"), "written").listFiles()
        if !dir.toString.contains("twitter") && dir.isDirectory;
        f <- dir.listFiles(new FilenameFilter {
      override def accept(dir: File, name: String): Boolean = name.endsWith(".txt") && !name.startsWith("easy_money") && !name.startsWith("sucker")
    })) yield {
      val slab = MascSlab(f.toURI.toURL)
      val slabWithSentences = MascSlab.s(slab)

      val guessPoints = potentialSentenceBoundariesIterator(slabWithSentences.content).toSet

      val endPoints = adjustEndPoints(slab.content, slabWithSentences.iterator[Sentence].map(_.end))
      numRight += (endPoints & guessPoints).size
      total += endPoints.size
      totalGuess += (guessPoints).size

      for(pos <- 0 until slab.content.length) {
        if(guessPoints(pos) && endPoints(pos)) print("[=")
        else if(endPoints(pos)) print("[[")
        else if(guessPoints(pos)) print("{{")
        print(slab.content.charAt(pos))
        if(guessPoints(pos) && endPoints(pos)) print("=]")
        else if(endPoints(pos)) print("]]")
        else if(guessPoints(pos)) print("}}")
      }

      for(p <- guessPoints if p < slab.content.length) {
        guessCounts(slab.content.charAt(p)) += 1
      }


      for(p <- endPoints) {
        goldCounts(slab.content.charAt(p)) += 1
      }


      for(missed <- (endPoints -- guessPoints)) {
        val beforeContext = slab.content.substring(math.max(0, missed - 10), missed)
        val afterContext = slab.content.substring(missed + 1, math.min(missed + 10, slab.content.length))
        val context = slab.content.charAt(missed)
        println(s"$f: $beforeContext[[$context]]$afterContext... ${Character.getType(context)} $missed")
      }

      /*
      for(missed <- (guessPoints --endPoints)) {
        assert(missed >= 0)
        val beforeContext = slab.content.substring(math.max(0, missed - 10), missed)
        println(missed + " " + slab.content.length)
        val afterContext = slab.content.substring(missed + 1, math.min(missed + 10, slab.content.length))
        val context = slab.content.charAt(missed)
        println(s"!!$f: $beforeContext[[$context]]$afterContext... ${Character.getType(context)}")
      }
      */

    }

    println(s"recall $numRight/$total : ${numRight * 1.0/total}")
    println(s"precision $numRight/$totalGuess : ${numRight * 1.0/totalGuess}")
    println(goldCounts)
    println(guessCounts)

  }
}
