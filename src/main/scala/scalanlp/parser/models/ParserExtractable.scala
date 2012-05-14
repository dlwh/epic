package scalanlp.parser.models

import scalanlp.epic.{Model, ModelFactory}
import scalala.tensor.dense.DenseVector
import scalanlp.parser.Parser
import scalanlp.trees.TreeInstance

/**
 *
 * @author dlwh
 */

trait ParserExtractable[L, W] {
  def extractParser(weights: DenseVector[Double]): Parser[L, W]
}


trait ParserExtractableModelFactory[L,W] extends ModelFactory[TreeInstance[L,W]] {
  type MyModel <: Model[TreeInstance[L,W]] with ParserExtractable[L,W]
}