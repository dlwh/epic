package epic.logo

import epic.framework.Example

class MulticlassClassifier[L, F, W](val weights : Weights[W], argmaxInferencer : MulticlassLossAugmentedArgmaxInferencer[L, F, W]) {

  def apply(fv : F) : L = {
    argmaxInferencer.argmax(weights, Example(null.asInstanceOf[L], fv))._1
  }

}
