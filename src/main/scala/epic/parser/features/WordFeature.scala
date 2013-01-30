package epic.parser.features

import epic.framework.Feature

/**
 * 
 * @author dlwh
 */
case class WordFeature(word: String, kind: Symbol) extends Feature

case class BigramFeature[W](a: W, b: W) extends Feature
case class TrigramFeature[W](a: W, b: W, c: W) extends Feature
