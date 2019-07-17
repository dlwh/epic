package epic.trees

case class PartialTreeProcessor() {
  import Trees.Transforms._
  private def ens = new EmptyNodeStripper[String]
  private def xox = new XOverXRemover[String]
 
  def apply(tree: Tree[String]):Tree[String] = {
    var transformed = xox(ens(tree).get)
    transformed = if (transformed.children.length != 1) {
      Tree("", IndexedSeq(transformed), transformed.span)
    } else {
      transformed
    }
    val ann = transformed.map { label =>
      val fields = AnnotatedLabel.parseTreebank(label)
      fields.label
    }
    ann
  }
}