package epic.trees.annotations
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
import epic.trees.{StandardTreeProcessor, Tree}


/**
 *
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class KMPipelineTest extends FunSuite {
  val processor = new StandardTreeProcessor()
  val pipeline = KMAnnotator()
  test("KLMA fig 7") {
    val (tree, words) = Tree.fromString("(TOP (S (NP (DT This)) (VP (VBZ is) (NP (NN panic) (NN buying))) (. .)))")
    val processed = processor(tree)
    val pipelined = pipeline(processed, words)
    import TreeAnnotations._
    // make sure we have a VPisVBF annotation
    assert(pipelined.allChildren.exists(t => t.label.baseLabel == "VP" && t.label.features.contains(VPisVBF)),"VPisVBF")
    // make sure the VP dominates a V
    assert(pipelined.allChildren.exists(t => t.label.baseLabel == "VP" && t.label.features.contains(Dom("V"))), "DomV")
    // make sure the S dominates a V
    assert(pipelined.allChildren.exists(t => t.label.label == "S" && t.label.features.contains(Dom("V"))), "DomV2")
    // make sure the @S dominates a V and has an NP to its left
    assert(pipelined.allChildren.exists(t => t.label.label == "@S" && t.label.features.contains(Dom("V")) && t.label.siblings(0) == Left("NP")), "DomV && NP left")
  }

}
