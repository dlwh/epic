package epic.coref

import epic.ontonotes.DSpan
import epic.everything.Property


trait PropertyExtractor[T] {
  def property: Property[T]
  /**
   * Returns index of choice. -1 for unknown
   */
  def extract(c: DSpan, context: CorefDocument):Int
}