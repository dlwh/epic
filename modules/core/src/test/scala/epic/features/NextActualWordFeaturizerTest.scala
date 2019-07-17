package epic.features

import breeze.linalg.{Axis, sum}
import org.scalatest.FunSuite
import epic.parser.ParserTestHarness

/**
 * TODO
 *
 * @author dlwh
 **/
class NextActualWordFeaturizerTest extends FunSuite {
  test("simple little test") {
    val ident = new IdentityWordFeaturizer[String](sum(ParserTestHarness.wordCounts._1, Axis._0))
    val next = new NextActualWordFeaturizer(ident, true)

    val identAnch = ident.anchor("This is a test , of the system .".split(" "))
    val anch = next.anchor("This is a test , of the system .".split(" "))
    assert(anch.featuresForWord(0).toIndexedSeq.collect { case ActualWordFeature(f, _) => f; case PunctuationFeature(f, _) => f} === identAnch.featuresForWord(0).toIndexedSeq)
    assert(anch.featuresForWord(1).toIndexedSeq.collect { case ActualWordFeature(f, _) => f; case PunctuationFeature(f, _) => f}  === identAnch.featuresForWord(1).toIndexedSeq)
    assert(anch.featuresForWord(4).toIndexedSeq.collect { case ActualWordFeature(f, _) => f; case PunctuationFeature(f, _) => f}  != identAnch.featuresForWord(4).toIndexedSeq)
    assert(anch.featuresForWord(4).toIndexedSeq.collect { case ActualWordFeature(f, _) => f; case PunctuationFeature(f, _) => f}  containsSlice identAnch.featuresForWord(5).toIndexedSeq)

  }

}
