package epic.trees

case class PartialTreeProcessor() {
  import Trees.Transforms._
  private def ens = new EmptyNodeStripper[String]
  private def xox = new XOverXRemover[String]
 
  def apply(tree: Tree[String]):Tree[String] = {
    var transformed = xox(ens(tree).get)
    transformed = if(transformed.children.length != 1) {
      Tree("", IndexedSeq(transformed), transformed.span)
    } else {
      transformed
    }
    val ann = transformed.map { label =>
      val fields = StandardTreeProcessor.splitLabel(label)
      fields(0);
//      val anno = fields.drop(1).filterNot(s => s.nonEmpty && s.charAt(0).isDigit)
//      interner.intern(AnnotatedLabel(fields.head.intern,
//        features= anno.iterator.map(tag => functionalTagInterner.intern(FunctionalTag(tag))).toSet
//      ))
    }
    ann;
  }
}