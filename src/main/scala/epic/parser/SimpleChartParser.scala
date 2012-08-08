package epic.parser
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
import epic.trees.BinarizedTree

@SerialVersionUID(1)
trait ChartParser[L, W] extends Parser[L, W] with Serializable {
  def charts(w: Seq[W]):ChartMarginal[ParseChart, L, W]

  def decoder: ChartDecoder[L, W]

  override def bestParse(w: Seq[W]):BinarizedTree[L] = try {
    val chart = charts(w)
    decoder.extractBestParse(chart)
  } catch {
    case e => throw e
  }
}

/**
 * A SimpleChartParser produces trees with labels C from a ChartBuilder with labels L, a decoder from C to L, and
 * projections from C to L
 * @author dlwh
 */
@SerialVersionUID(1)
class SimpleChartParser[L, W](val augmentedGrammar: AugmentedGrammar[L, W],
                              val decoder: ChartDecoder[L, W],
                              val maxMarginals: Boolean = false) extends ChartParser[L, W] with Serializable {

  def charts(w: Seq[W]) = try {
    ChartMarginal(augmentedGrammar, w, if(maxMarginals) ParseChart.viterbi else ParseChart.logProb)
  } catch {
    case e =>
      try {
        ChartMarginal(AugmentedGrammar.fromRefined(augmentedGrammar.refined), w, if(maxMarginals) ParseChart.viterbi else ParseChart.logProb)
      } catch {
        case e =>
        throw e
      }

  }
  def grammar = augmentedGrammar.grammar

}

object SimpleChartParser {
  def apply[L, W](grammar: AugmentedGrammar[L, W], viterbi: Boolean = false) = {
    if(!viterbi)
      new SimpleChartParser[L, W](grammar, new MaxRuleProductDecoder(grammar.grammar, grammar.lexicon), false)
    else
     new SimpleChartParser[L, W](grammar, new ViterbiDecoder, false)
  }

}
