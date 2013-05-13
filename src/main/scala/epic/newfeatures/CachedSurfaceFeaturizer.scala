package epic.newfeatures

import com.google.common.collect.MapMaker

/**
 *
 * @author dlwh
 */
class CachedSurfaceFeaturizer[W](val base: SurfaceFeaturizer[W]) extends SurfaceFeaturizer[W] {
  private val cache = new MapMaker().softValues().makeMap[IndexedSeq[W], SurfaceFeatureAnchoring[W]]()
  def anchor(words: IndexedSeq[W]): SurfaceFeatureAnchoring[W] = {
    val cached = cache.get(words)
    if(cached ne null) cached
    else {
      val x = base.anchor(words)
      cache.put(words, x)
      x
    }
  }
}
