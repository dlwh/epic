package epic.features

import epic.framework.Feature
import breeze.util.CachedHashCode

/**
 * TODO
 *
 * @author dlwh
 **/
case class LabelFeature[L](l: L) extends Feature with CachedHashCode
