package scalanlp.epic

/**
 *
 * @author dlwh
 */
trait ModelFactory[Datum] {
  type MyModel <: Model[Datum]

  def make(train: IndexedSeq[Datum]): MyModel
}