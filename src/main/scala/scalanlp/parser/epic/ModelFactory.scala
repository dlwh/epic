package scalanlp.parser.epic

import scalala.tensor.Counter2

/**
 * 
 * @author dlwh
 */
trait ModelFactory[Datum] {
  type MyModel <: Model[Datum]
  def make(train: IndexedSeq[Datum]):MyModel
}