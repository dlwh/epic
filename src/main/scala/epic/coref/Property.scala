package epic.coref

import breeze.util.Index

/**
 * Basically just a multinomial valued property. Entities
 * in the system will have one of these,
 */
trait Property {
  def choices: Index[Symbol]
  def name: String
  def arity = choices.size
  override def toString = choices.mkString(name +"[", ", ", "]")
}

object Property {
  def apply(name: String)(choices: Symbol*):Property = {
    val c = Index(choices)
    val n = name
    new Property {
      def choices = c
      def name = n
    }
  }
}

trait PropertyExtractor {
  def property: Property
  /**
   * Returns index of choice. -1 for unknown
   */
  def extract(c: MentionCandidate, context: CorefDocument):Int
}
