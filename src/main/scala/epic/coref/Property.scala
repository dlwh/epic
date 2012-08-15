package epic.coref

import breeze.util.Index

/**
 * Basically just a multinomial valued property. Entities
 * in the system will have one of these,
 */
trait Property {
  def choices: Index[String]
  def name: String
  override def toString = choices.mkString(name +"[", ", ", "]")
}

object Property {
  def apply(name: String)(choices: String*):Property = {
    val c: Index[String] = Index(choices)
    val n = name
    new Property {
      def choices = c
      def name = n
    }
  }
}
