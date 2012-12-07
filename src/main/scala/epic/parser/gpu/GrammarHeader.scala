package epic.parser.gpu

object GrammarHeader {
  val SCALE_FACTOR = 12
  def header[L](rules: RuleStructure[L], numGrammars: Int = 1) = {
    import rules._
    val byParent = rules.binaryRulesWithIndices.groupBy(_._1.parent).values.map(_.size).max
    """#define SCALE_FACTOR %d
#define NUM_SYMS %d
#define NUM_GRAMMARS %d
#define ROOT %d
#define NUM_BINARY %d
#define NUM_UNARY %d
#define MAX_NUM_RULES_PER_SYMBOL %d
#define TRIANGULAR_INDEX(begin, end) ((end) * ((end)+1)/2 + begin)
#define CELL(chart, begin, end)   ((chart) + (TRIANGULAR_INDEX(begin, end)))

typedef struct {
  float binaries[NUM_BINARY][NUM_GRAMMARS];
  float unaries[NUM_UNARY][NUM_GRAMMARS];
} rule_cell;

typedef struct {
  float syms[NUM_SYMS][NUM_GRAMMARS];
} parse_cell;
    """.format(SCALE_FACTOR, numSyms, numGrammars, root, numBinaries, numUnaries, byParent)
  }}
