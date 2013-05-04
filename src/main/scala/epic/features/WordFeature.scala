package epic.features

import epic.framework.Feature

/**
 * 
 * @author dlwh
 */
case class WordFeature(word: Any, kind: Symbol) extends Feature

case class PrevWordFeature[W](a: W) extends Feature
case class NextWordFeature[W](a: W) extends Feature
case class BigramFeature[W](a: W, b: W) extends Feature
case class TrigramFeature[W](a: W, b: W, c: W) extends Feature
