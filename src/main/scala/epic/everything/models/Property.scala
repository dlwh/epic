package epic.everything.models

import breeze.util.Index

/**
 * Basically just a multinomial valued property. Entities
 * in the system will have one of these,
 */
case class Property[T](name: String, choices: Index[T]) {
  def arity = choices.size

  override def toString = choices.mkString(name + "[", ", ", "]")
}



object Property {
  def apply[T](name: String)(choices: T*):Property[T] = {
    new Property(name, Index(choices))
  }
}
