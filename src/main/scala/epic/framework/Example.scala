package epic.framework

trait Example[+L,+T] extends Observation[T] with Labeled[L] with Serializable {outer=>
  def id : String
  def label: L

  /**
   * Converts the features in this example to a different one while still
   * preserving label and id.
   */
  override def map[U](f: T=>U):Example[L, U] = new Example[L,U] {
    val label = outer.label
    val id = outer.id
    val features = f(outer.features)
  }

  /**
   * Converts the label in this example to a different one while still
   * preserving features and id.
   */
  def relabel[L2](f: L=>L2):Example[L2, T] = new Example[L2,T] {
    val label = f(outer.label)
    val id = outer.id
    val features = outer.features
  }

  override def toString = {
    "Example { id =" + id + ", label = " + label + ", features = " + features + "}"
  }

}

object Example {

  /**
   * Create a new Example.
   */
  def apply[L,T](label: L, features: T, id:String=""): Example[L,T] = {
    val l = label
    val f = features
    val i = id
    new Example[L,T] {
      val id = i
      val label = l
      val features = f
    }
  }

  /**
   * Lifts a function to operate over Examples,
   * Rather than the contained object.
   */
  def lift[T,U,L](f: T=>U) = (o : Example[L,T]) => o.map(f)

}

/**
 * Something that has a label.
 *
 * @author dlwh
 */
trait Labeled[+L] {
  def label : L
}

trait Observation[+T] extends Serializable { outer=>
  def id : String
  def features: T

  /**
   * strict, but cached, transformation of features
   */
  def map[U](f: T=>U):Observation[U] = new Observation[U] {
    val id = outer.id
    val features = f(outer.features)
  }

  override def toString = {
    "Observation { ids =" + id + ", features = " + features + "}"
  }
}

object Observation {

  /**
   * Create an observation.
   */
  def apply[T](_features: T, _id: String = "") = new Observation[T] {
    val id = _id
    val features = _features
  }

  /**
   * Lifts a function to operate over Observations,
   * Rather than the contained object.
   */
  def lift[T,U](f: T=>U) = (o : Observation[T]) => o.map(f)

}
