package epic.everything.models

import breeze.util.{Encoder, Index}
import breeze.inference.bp.Variable

/**
 * Basically just a multinomial valued property. Entities
 * in the system will have one of these,
 */
final case class Property[T](name: String, choices: Index[T]) extends Encoder[T] {
  def size: Int = arity

  def toVariable = Variable(name, choices)
  def arity = choices.size
  val index = choices

  override def toString = choices.mkString(name + "[", ", ", "]")
}



object Property {

  def apply[T](name: String)(choices: T*):Property[T] = {
    new Property(name, Index(choices))
  }

}
