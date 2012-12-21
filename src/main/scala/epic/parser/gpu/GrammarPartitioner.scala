package epic.parser.gpu

import epic.trees.BinaryRule
import collection.mutable
import collection.immutable.BitSet

object GrammarPartitioner {
  sealed trait TargetLabel {
    def clusterPieces(r: BinaryRule[Int]) = this match {
      case Parent => BitSet(r.left) -> BitSet(r.right)
      case LeftChild => BitSet(r.parent) -> BitSet(r.right)
      case RightChild => BitSet(r.parent) -> BitSet(r.left)
    }

    def target(r: BinaryRule[Int]) = this match {
      case Parent => r.parent
      case LeftChild => r.left
      case RightChild => r.parent
    }
  }
  case object Parent extends TargetLabel
  case object LeftChild extends TargetLabel
  case object RightChild extends TargetLabel

  def partition(rules: IndexedSeq[(BinaryRule[Int], Int)],
                maxPartitionLabelSize: Int = 45,
                numRestarts: Int = 50,
                targetLabel: TargetLabel = Parent) = {

    case class Partition(targets: BitSet, group1: BitSet, group2: BitSet, isPure: Boolean = true) {
      def merge(p: Partition) = Partition(targets ++ p.targets, group1 ++ p.group1, group2 ++ p.group2, false)

      def tSize = targets.size

      def badness = group1.size + group2.size


      def isTooBig = !isPure && (group1.size + group2.size + targets.size) > maxPartitionLabelSize
    }
    var clusters_x = rules.groupBy(r => targetLabel.target(r._1))

     val initialClusters = clusters_x.map { case (p:Int, r: IndexedSeq[(BinaryRule[Int], Int)]) =>
        val (g1, g2) = r.map(rr => targetLabel.clusterPieces(rr._1)).unzip
        p -> Partition(BitSet(p), g1.reduce( _ ++ _), g2.reduce(_ ++ _))
      }
    def restart(random: =>Double) = {

      var clusters = initialClusters.map { case (k,v) => BitSet(k) -> v}

      def remove(p: Partition, t: Int) = {
        (for(t2 <- p.targets if t != t2) yield initialClusters(t2)).reduceLeft(_ merge _)
      }

      sealed trait Action { def priority: Double}
      case class Merge(p1: Partition, p2: Partition, merged: Partition) extends Action {
        val priority = (p1.badness + p2.badness - merged.badness)*random
      }
      case class SplitMerge(p1: Partition, p2: Partition, t: Int) extends Action {
        val newP1 = remove(p1, t)
        val newP2 = p2 merge initialClusters(t)
        val priority = (p1.badness + p2.badness - newP1.badness - newP2.badness)*random
      }

      implicit val order = Ordering[Double].on[Action](_.priority)

      val queue = new mutable.PriorityQueue[Action]
      queue ++= {for(p1 <- clusters.values; p2 <- clusters.values if p1 != p2) yield Merge(p1, p2, p1 merge p2)}

      while(queue.nonEmpty) {
        queue.dequeue() match {
          case sm@Merge(l, r, merger) =>
            if(clusters.contains(l.targets) && clusters.contains(r.targets)) {
              if(!merger.isTooBig) {
                clusters -= l.targets
                clusters -= r.targets
                queue ++= {for(p2 <- clusters.values) yield Merge(merger, p2, merger merge p2)}
                queue ++= {for(p2 <- clusters.values; rm  <- merger.targets) yield SplitMerge(merger, p2, rm)}
                clusters += (merger.targets -> merger)

              }
            }
          case sm@SplitMerge(l, r, _) =>
            if(clusters.contains(l.targets) && clusters.contains(r.targets)) {
              import sm._
              if(!newP2.isTooBig) {
                clusters -= l.targets
                clusters -= r.targets
                queue ++= {for(p2 <- clusters.values) yield Merge(newP1, p2, newP1 merge p2)}
                queue ++= {for(p2 <- clusters.values) yield Merge(newP2, p2, newP2 merge p2)}
                queue ++= {for(p2 <- clusters.values; rm  <- newP1.targets if newP1.targets.size > 1) yield SplitMerge(newP1, p2, rm)}
                queue ++= {for(p2 <- clusters.values; rm  <- newP2.targets if newP2.targets.size > 1) yield SplitMerge(newP2, p2, rm)}
                clusters += (newP1.targets -> newP1)
                clusters += (newP2.targets -> newP2)

              }
            }
        }
      }

      clusters
    }

    val clusters = ((0 until numRestarts).par.view.map(new java.util.Random(_)).map(r => restart(0.3 + 0.8 * r.nextDouble())) :+ restart(1.0)).minBy(_.values.map(_.badness).sum)

    println("Best badness: " + targetLabel  + " " + clusters.values.map(_.badness).sum)

    var p = 0
    for( Partition(targets, g1, g2, _) <- clusters.values) {
      println("Partition " + p)
      println("G1: " + g1.size + " " + g1)
      println("G2: " + g2.size + " "  + g2)
      println("targets: " + targets)
      p += 1
    }

    assert(clusters.values.flatMap(_.targets).toSet.size == clusters_x.keySet.size)
    clusters.values.map(p => p.targets.flatMap(clusters_x).toIndexedSeq)
  }

}
