package epic.logo

import epic.framework.Example

case class MulticlassClassifier[L, F, W](weights: Weights[W],
                                         argmaxInferencer: MulticlassLossAugmentedArgmaxInferencer[L, F, W]) {

  def apply(fv : F) : L = {
    // TODO this is gross, really need to separate unlabeled examples from labeled examples better
    val fakeLabel = null.asInstanceOf[L]
    argmaxInferencer.argmax(weights, Example(fakeLabel, fv))._1
  }

}
