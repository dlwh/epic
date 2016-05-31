package epic.features

import breeze.util.Index

/**
 * TODO
 *
 * @author dlwh
 **/
class NormalIndexBuilder[F] extends IndexBuilder[F] {

  private val _result = Index[F]()

  def result():Index[F] = _result
  
  def add(fs: TraversableOnce[F]):Unit = {
    fs.foreach(_result.index)
  }

}

trait IndexBuilder[F] {
  def result():Index[F]
  def add(fs: TraversableOnce[F]):Unit
}
