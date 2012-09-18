package epic.coref

import epic.everything.models.Property


trait PropertyExtractor {
  def property: Property
  /**
   * Returns index of choice. -1 for unknown
   */
  def extract(c: MentionCandidate, context: CorefDocument):Int
}