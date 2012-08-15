package epic.coref
import java.util.Arrays

/**
 *
 * @param marginals mention -> property -> value -> score
 * @param messages mention_from -> message_tp -> property -> value -> score
 */
case class PropertyBeliefs(marginals: Array[Array[Array[Double]]], messages: Array[Array[Array[Array[Double]]]])

object PropertyBeliefs {
  def forInstance(properties: Array[Property], inst: IndexedCorefInstance) = {
    val marginals = inst.properties.map { myProperties =>
      Array.tabulate(properties.length){ p =>
        val probs = new Array[Double](properties(p).choices.size)
        if (myProperties(p) >= 0) {
          probs(p) = 1.0
        } else {
          Arrays.fill(probs, 1.0/probs.length)
        }
        probs
      }
    }

    val messages = Array.fill(marginals.length){
      val to = inst.properties.map { myProperties =>
        Array.tabulate(properties.length){ p =>
          val probs = new Array[Double](properties(p).choices.size)
          Arrays.fill(probs, 1.0)
          probs
        }
      }
      to
    }

    new PropertyBeliefs(marginals, messages)
  }
}
