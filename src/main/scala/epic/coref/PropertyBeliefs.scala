package epic.coref
import java.util.Arrays

/**
 *
 * @param marginals mention -> property -> value -> score
 * @param messages mention_from -> message_tp -> property -> value -> score
 */
case class PropertyBeliefs(marginals: Array[Array[Array[Double]]], messages: Array[Array[Array[Array[Double]]]])


