The neural CRF parser is a high-performing constituency parser.



## Preamble

The neural CRF parser is described in:

"Neural CRF Parsing" Greg Durrett and Dan Klein. ACL 2015.

It is an extension of the span parser described in

"Less Grammar, More Features" David Hall, Greg Durrett, and Dan Klein. ACL 2014.

and is based on the Epic parsing framework. See https://github.com/dlwh/epic
for more documentation about the span parser and the Epic framework.
See http://www.eecs.berkeley.edu/~gdurrett/ for papers and BibTeX.

Questions? Bugs? Email me at gdurrett@eecs.berkeley.edu



## Setup

You need three things to run the neural CRF parser:

1) The compiled .jar; run ```sbt assembly``` to produce this

2) A treebank: the Penn Treebank or one of the SPMRL treebanks

3) Some sort of word vectors. These can either be in the .bin format
of Mikolov et al. (2013) or the .txt format of Bansal et al. (ACL 2014).  For
English, the best performance comes from using Bansal et al.'s vectors:

http://ttic.uchicago.edu/~mbansal/codedata/dependencyEmbeddings-skipdep.zip

For other languages, you can train suitable vectors on monolingual data using
```word2vec``` with the following arguments:

    -cbow 0 -size 100 -window 1 -sample 1e-4 -threads 8 -binary 0 -iter 15

These are mildly tuned, and using a small window size is important, but other
settings are likely to work well too.




## Usage

To run the parser on new text (tokenized, one-sentence-per-line), use the following command:

    java -Xmx4g -cp path/to/assembly.jar epic.parser.ParseText --model neuralcrf.parser \
      --tokenizer whitespace --sentences newline --nthreads 8 [files]

You can download the ```neuralcrf.parser``` model from:

http://nlp.cs.berkeley.edu/projects/neuralcrf.shtml

(As of March 1, 2017, this model does not work with the latest version of epic. If you want
to use the pre-trained model, use the commit with hash

8968e0966da28101744ce6f5bbb0de4345d9c594

from March 30, 2016.)

To train a new parser as described in the neural CRF paper, run the following command
(note that you need to fill in paths for -cp, --treebank.path, and --word2vecPath):

    java -Xmx47g -cp path/to/assembly.jar epic.parser.models.NeuralParserTrainer \
      --cache.path constraints.cache \
      --opt.useStochastic \
      --treebank.path path/to/wsj/ \
      --evalOnTest \
      --includeDevInTrain \
      --trainer.modelFactory.annotator epic.trees.annotations.PipelineAnnotator \
      --ann.0 epic.trees.annotations.FilterAnnotations  \
      --ann.1 epic.trees.annotations.ForgetHeadTag \
      --ann.2 epic.trees.annotations.Markovize \
      --ann.2.horizontal 0 \
      --ann.2.vertical 0 \
      --modelFactory epic.parser.models.PositionalNeuralModelFactory \
      --opt.batchSize 200 \
      --word2vecPath path/to/skipdep_embeddings.txt \
      --threads 8

To run on SPMRL treebanks, modify the arguments to the command above as follows:

1) Add the following arguments (replace ${LANG}$ as appropriate):

    --treebankType spmrl \
    --binarization head \
    --supervisedHeadFinderPtbPath path/to/gold/ptb/train/train.${LANG}.gold.ptb \
    --supervisedHeadFinderConllPath path/to/gold/conll/train/train.${LANG}.gold.conll \
    --ann.3 epic.trees.annotations.SplitPunct

2) Modify --treebank.path to point to the X_SPMRL/gold/ptb directory.

Options to configure the neural network and training are largely defined in ```epic.parser.models.PositionalNeuralModelFactory```

### Miscellaneous Notes

To run on the development set, simply remove ```evalOnTest``` and
```includeDevInTrain``` from the arguments.

You should use the official version of ```evalb``` on the output files (gold
and guess) rather than relying on the native scorer in the Epic parser. For
SPMRL, you should use the version distributed with the shared task.

Note that the X-bar grammar and coarse pruning masks (constraints) are cached
between runs in the same directory, which speeds up training and testing time
considerably as generating the masks is time-consuming.

Finally, note that multiple parsers cannot be trained simultaneously in
the same directory, since certain files (such as pruning masks from the
coarse model) will collide.

