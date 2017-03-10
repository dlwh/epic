package epic.util

object TwoTwelveSupport {
  def _equals(x: Product, y: Any): Boolean = y match {
    case y: Product if x.productArity == y.productArity => x.productIterator sameElements y.productIterator
    case _                                              => false
  }
}
