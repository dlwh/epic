package epic.parser.models

import epic.framework.{Model, ModelFactory}
import breeze.linalg._
import epic.parser.Parser
import epic.trees.TreeInstance

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