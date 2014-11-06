package epic.parser
package projections
/*
 Copyright 2012 David Hall

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit._
import org.scalatest.prop._
import epic.trees.{TreeInstance, AnnotatedLabel}


/**
 *
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class EnumeratedAnchoringTest  extends FunSuite with ParserTestHarness {

  test("We can parse using simple anchoring") {
    val gen = ParserTestHarness.simpleParser
    val f = new AnchoredPCFGProjector[AnnotatedLabel, String](Double.NegativeInfinity)

    val grammar = new ProjectingCoreGrammar(gen, f)
    val chartParser = Parser(grammar)

    val grammarNext = new ProjectingCoreGrammar(chartParser, f)
    val chartNext = Parser(grammarNext)

    for( TreeInstance(_, t, w) <- getTestTrees()) try {
      val tree1 = chartParser(w)
      val tree2 = chartNext(w)
      assert(tree2 === tree1, "late")
    } catch {
      case e: Exception =>
      throw new RuntimeException("Trouble with " + t.render(w), e)
    }

  }

  test("Parsing kind of works using it") {
    val gen = ParserTestHarness.simpleParser
    val f = new AnchoredPCFGProjector[AnnotatedLabel, String](Double.NegativeInfinity)
    val grammar = new ProjectingCoreGrammar(gen, f)

    val chartParser = Parser(grammar)

    val res = evalParser(getTestTrees(), chartParser)
    assert(res.f1 > 0.5, res.f1)

  }

}