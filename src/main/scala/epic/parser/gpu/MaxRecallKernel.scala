package epic.parser.gpu

import com.nativelibs4java.opencl._
import collection.mutable.ArrayBuffer
import java.lang.{Float=>JFloat, Integer=>JInt}

class MaxRecallKernel[Coarse](rules: RuleStructure[Coarse])(implicit context: CLContext) {
  def makeBackpointers(backPointers: CLBuffer[JInt],
                       projectedTop: CLBuffer[JFloat],
                       projectedBot: CLBuffer[JFloat],
                       offsets: CLBuffer[JInt],
                       lengths: CLBuffer[JInt],
                       maxLength: Int,
                       events: CLEvent*)(implicit queue: CLQueue) = synchronized {
    max_recall.setArgs(backPointers, projectedTop, projectedBot, offsets, lengths, Integer.valueOf(1))
    val mr = new ArrayBuffer[CLEvent]()

    var event = max_recall.enqueueNDRange(queue, Array(lengths.getElementCount.toInt, maxLength), events:_*)
    for (len <- 2 to maxLength) {
      max_recall.setArg(5, len)
      event = max_recall.enqueueNDRange(queue, Array(lengths.getElementCount.toInt, maxLength + 1 - len), event)
    }

    event
  }


  private lazy val max_recall = program.createKernel("max_recall")

  val program = {
    val p = context.createProgram(text)
    p.setFastRelaxedMath()
    p.setUnsafeMathOptimizations()
    p.build()
  }

  lazy val text = GrammarHeader.header(rules, 1) +
    """
typedef struct {
  int top, bot, split;
  float score;
} backpointer;

__kernel void max_recall(
              __global backpointer* backpointers,
              __global const parse_cell * marg_tops,
              __global const parse_cell * marg_bots,
              __global const int* offsets,
              __global const int* lengths,
              const int spanLength) {
  const int sentence = get_global_id(0);
  const int begin = get_global_id(1);
  const int gram = 0;
  const int end = begin + spanLength;
  const int length = lengths[sentence];
  float maxSymBotV = 0.0f;
  int maxSymBot = -1;
  float maxSymTopV = 0.0f;
  int maxSymTop = -1;
  __global backpointer* back = backpointers + offsets[sentence];

  if (end <= length) {
    __global backpointer* bp = CELL(back, begin, end);
    __global const parse_cell* mb = CELL(marg_bots + offsets[sentence], begin, end);
    __global const parse_cell* mt = CELL(marg_tops + offsets[sentence], begin, end);
    for(int i = 0; i < NUM_SYMS; ++i) {
      float score = mb->syms[i][gram];
      if(score > maxSymBotV) {
        maxSymBotV = score;
        maxSymBot = i;
      }
      float score2 = mt->syms[i][gram];
      if(score2 > maxSymTopV) {
        maxSymTopV = score2;
        maxSymTop = i;
      }
    }
    bp->top = maxSymTop;
    bp->bot = maxSymBot;
    int bestSplit = -1;
    float bestScore = 0.0;
    for(int split = begin + 1; split < end; ++split) {
      __global const backpointer* left = CELL(back, begin, split);
      __global const backpointer* right = CELL(back, split, end);

      float score = left->score + right->score;
      if(score > bestScore) {
        bestSplit = split;
        bestScore = score;
      }
    }
    if(bestScore == 0.0 && spanLength != 1)
      bp->score = -10000.0f;
    else
      bp->score = bestScore + maxSymBotV + maxSymTopV;
    bp->split = bestSplit;
  }
}"""
}
