package epic.coref

import epic.everything.models.Property
import epic.everything.DSpan


trait PropertyExtractor[T] {
  def property: Property[T]
  /**
   * Returns index of choice. -1 for unknown
   */
  def extract(c: DSpan, context: CorefDocument):Int
}