package epic.parser.gpu

object GrammarHeader {
  val SCALE_FACTOR = 10
  def header[C, L](rules: RuleStructure[C, L], numGrammars: Int = 1) = {
    import rules._
    val byParent = rules.binaryRulesWithIndices.groupBy(_._1.parent).values.map(_.size).max
    """
#define SCALE_FACTOR %d
#define NUM_SYMS %d
#define NUM_GRAMMARS %d
#define ROOT %d
#define NUM_BINARY %d
#define NUM_UNARY %d
#define MAX_NUM_RULES_PER_SYMBOL %d
#define TRIANGULAR_INDEX(begin, end) ((end) * ((end)+1)/2 + begin)
#define CELL(chart, begin, end)   ((chart) + (TRIANGULAR_INDEX(begin, end)))

#define NUM_PROJECTED_SYMS  %d


typedef struct {
  float binaries[NUM_BINARY][NUM_GRAMMARS];
  float unaries[NUM_UNARY][NUM_GRAMMARS];
} rule_cell;

typedef struct {
  float syms[NUM_SYMS][NUM_GRAMMARS];
} parse_cell;

typedef struct {
  float syms[NUM_PROJECTED_SYMS][NUM_GRAMMARS];
} projected_parse_cell;

typedef struct {
  unsigned long allowed[%d];
} pruning_mask;

#define COARSE_IS_SET(mask, coarse) ( ((mask).allowed[(coarse)>>6]&(1L<<(coarse&63))) != 0)
#define SET_COARSE(mask, coarse) ((mask).allowed[(coarse)>>6] |= (1L<<(coarse&63)))
#define IS_ANY_SET(mask) %s

    """.format(SCALE_FACTOR, numSyms, numGrammars, root, numBinaries, numUnaries, byParent, numCoarseSyms, pruningMaskFieldSize, Array.tabulate(pruningMaskFieldSize)("(mask).allowed[%d]" format _).mkString("((", "|", ") != 0)"))
  }}
