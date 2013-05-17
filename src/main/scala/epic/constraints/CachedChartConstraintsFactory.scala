package epic.constraints

/*
 Copyright 2012 David Hall

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/
import epic.util.CacheBroker

/**
 * A CoreGrammar that relies on a file cache, which stores
 * a Map[IndexedSeq[W], CoreAnchoring] and a backoff grammar.
 * Currently, only [[epic.parser.projections.ProjectTreebankToConstraints]]
 * creates these.
 *
 * @author dlwh
 */
@SerialVersionUID(1L)
class CachedChartConstraintsFactory[L, W](backoff: ChartConstraints.Factory[L, W],  name: String = "parseConstraints")(implicit broker: CacheBroker) extends ChartConstraints.Factory[L, W] with Serializable {
  private val cache = broker.make[IndexedSeq[W], ChartConstraints[L]](name)
  def constraints(w: IndexedSeq[W]): ChartConstraints[L] = cache.getOrElseUpdate(w, backoff.constraints(w))
}
