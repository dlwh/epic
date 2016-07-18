# Epic

(c) 2014 David Hall.

Epic is a structured prediction framework for Scala. It also includes classes for training high-accuracy syntactic parsers, part-of-speech taggers, name entity recognizers, and more.

Epic is distributed under the [Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0.html).

The current version is 0.3.

## Documentation

Documentation will (eventually) live at the GitHub wiki: <https://github.com/dlwh/epic/wiki>

See some example usages at <https://github.com/dlwh/epic-demo>.

## Using Epic

Epic can be used programmatically or from the command line, using
either pretrained models ([see below](#pre-trained-models)) or with
models you have trained yourself. 

Currently, Epic has support for three kinds of models: parsers, sequence
labelers, and segmenters. Parsers produce syntactic representations
of sentences. Sequence labelers are things like part-of-speech
taggers. These associate each word in a sentence with a label. For
instance, a part-of-speech tagger can identify nouns, verbs, etc.
Segmenters break a sentence into a sequence of fields. For instance,
a named entity recognition system might identify all the people,
places and things in a sentence.

### Command-line Usage

Epic bundles command line interfaces for using parsers, NER systems, and POS taggers (and more generally, segmentation and tagging systems). There are three classes, one for each kind of system:

* `epic.parser.ParseText` runs a parser.
* `epic.sequences.SegmentText` runs an NER system, or any kind of segmentation system.
* `epic.sequences.TagText` runs a POS tagger, or any kind of tagging system.

All of these systems expect plain text files as input, along with a path to a model file. The syntax is:

```bash
java -Xmx4g -cp /path/to/epic-assembly-0.3-SNAPSHOT.jar epic.parser.ParseText --model /path/to/model.ser.gz --nthreads <number of threads> [files]
```

Currently, all text is output to standard out. In the future, we will support output in a way that differentiates the files. If no files are given, the system will read from standard input. By default, the system will use all available cores for execution.

Models can be downloaded from <http://www.scalanlp.org/models/> or from Maven Central. ([See below](#pre-trained-models).)

### Programmatic Usage

Epic also supports programmatic usage. All of the models assume that text has been segmented and tokenized.

#### Preprocessing text

To preprocess text so that the models can use them, you will need to segment out sentences and tokenize the sentences into individual words. Epic comes with classes to do both.

Once you have a sentence, you can tokenize it using a `epic.preprocess.TreebankTokenizer`, which takes a string and returns a sequence of tokens. All told, the pipeline looks like this:

```scala
val text = getSomeText();

val sentenceSplitter = MLSentenceSegmenter.bundled().get
val tokenizer = new epic.preprocess.TreebankTokenizer()

val sentences: IndexedSeq[IndexedSeq[String]] = sentenceSplitter(text).map(tokenizer).toIndexedSeq

for(sentence <- sentences) {
  // use the sentence tokens
}

```



#### Parser

To use the parser programmaticaly, deserialize a parser model--either using `epic.models.deserialize[Parser[AnnotatedLabel, String]](path)` or using the ParserSelector. Then, give the parser segmented and tokenized text:

```scala
val parser = epic.models.deserialize[Parser[AnnotataedLabel, String]](path)

// or:

val parser = epic.models.ParserSelector.loadParser("en").get // or another 2 letter code.

val tree = parser(sentence)

println(tree.render(sentence))

```

Trees have a number of methods on them. See the class definition or [API docs](http://www.scalanlp.org/api/epic).

#### Part-of-Speech Tagger

Using a Part-of-Speech tagger is similar to using a parser: load a model, tokenize some text, run the tagger. All taggers are (currently) [linear chain conditional random fields](http://people.cs.umass.edu/~mccallum/papers/crf-tutorial.pdf), or CRFs. (You don't need to understand them to use them. They are just a machine learning method for assigning a sequence of tags to a sequence of words.)

```scala
val tagger = epic.models.deserialize[CRF[AnnotatedLabel, String]](path)

// or:

val tagger = epic.models.PosTagSelector.loadTagger("en").get // or another 2 letter code.

val tags = tagger.bestSequence(sentence)

println(tags.render)

```


#### Named Entity Recognition

Using a named entity recognizer is similar to using a pos tagger: load a model, tokenize some text, run the recognizer. All NER systems are (currently) [linear chain semi-Markov conditional random fields](http://people.cs.umass.edu/~mccallum/papers/crf-tutorial.pdf), or SemiCRFs. (You don't need to understand them to use them. They are just a machine learning method for segmenting text into fields.)

```scala
val ner = epic.models.deserialize[SemiCRF[AnnotatedLabel, String]](path)

// or:

val ner = epic.models.NerSelector.loadNer("en").get// or another 2 letter code.

val segments = ner.bestSequence(sentence)

println(segments.render)

```

The outside label of a SemiCRF is the label that is consider not part of a "real" segment. For instance, in NER, it is the label given to words that are not named entities.

### Pre-trained Models

Epic provides a number of pre-trained models. These are available as Maven artifacts from Maven Central, and can be loaded at runtime. To use a specific model, just depend on it (or alternatively download the jar file). You can then load the parser by calling, for example:

```scala
epic.parser.models.en.span.EnglishSpanParser.load()
```

This will load the  model and return a `Parser` object. If you want to not hardwire dependencies, either for internationalization or to potentially try different models, use `epic.models.ParserSelector.loadParser(language)`, where
language is the [two letter code for the language](http://www.loc.gov/standards/iso639-2/php/code_list.php) you want to use.

To following models are available at this time:

__AS OF WRITING ONLY MODELS FOR ENGLISH ARE AVAILABLE!__ Write me if you want these other models.

* Parser
  * English: 
    ```
    "org.scalanlp" %% "epic-parser-en-span" % "2015.1.25"
    ```
* POS Taggers
  * English: 
    ```
    "org.scalanlp" %% "epic-pos-en" % "2015.1.25"
    ```
* Named Entity Recognizers
  * English: 
    ```
    "org.scalanlp" %% "epic-ner-en-conll" % "2015.1.25"
    ```

There is also a meta-dependency that includes the above three models:

```
"org.scalanlp" %% "english"  % "2015.1.25"
```

I meant to name that "epic-english" but messed up. So it's that for now. Expect it to change.

TODO:

* Parser
  * English: 
    ```
    "org.scalanlp" %% "epic-parser-en-span" % "2014.9.15-SNAPSHOT"
    ```
  * Basque: 
    ```
    "org.scalanlp" %% "epic-parser-eu-span" % "2014.9.15-SNAPSHOT"
    ```
  * French: 
    ```
    "org.scalanlp" %% "epic-parser-fr-span" % "2014.9.15-SNAPSHOT"
    ```
  * German: 
    ```
    "org.scalanlp" %% "epic-parser-de-span" % "2014.9.15-SNAPSHOT"
    ```
  * Hungarian: 
    ```
    "org.scalanlp" %% "epic-parser-hu-span" % "2014.9.15-SNAPSHOT"
    ```
  * Korean: 
    ```
    "org.scalanlp" %% "epic-parser-ko-span" % "2014.9.15-SNAPSHOT"
    ```
  * Polish:
    ```
    "org.scalanlp" %% "epic-parser-pl-span" % "2014.9.15-SNAPSHOT"
    ```
  * Swedish: 
    ```
    "org.scalanlp" %% "epic-parser-sv-span" % "2014.9.15-SNAPSHOT"
    ```
* POS Taggers
  * Basque: 
    ```
    "org.scalanlp" %% "epic-pos-eu" % "2014.9.15-SNAPSHOT"
    ```
  * French: 
    ```
    "org.scalanlp" %% "epic-pos-fr" % "2014.9.15-SNAPSHOT"
    ```
  * German: 
    ```
    "org.scalanlp" %% "epic-pos-de" % "2014.9.15-SNAPSHOT"
    ```
  * Hungarian: 
    ```
    "org.scalanlp" %% "epic-pos-hu" % "2014.9.15-SNAPSHOT"
    ```
  * Polish: 
    ```
    "org.scalanlp" %% "epic-pos-pl" % "2014.9.15-SNAPSHOT"
    ```
  * Swedish: 
    ```
    "org.scalanlp" %% "epic-pos-sv" % "2014.9.15-SNAPSHOT"
    ```
* Named Entity Recognizers
  * English: 
    ```
    "org.scalanlp" %% "epic-ner-en-conll" % "2014.9.15-SNAPSHOT"
    ```


If you use any of the parser models in research publications, please cite:

> David Hall, Greg Durrett, and Dan Klein. 2014. Less Grammar, More Features. In ACL.

If you use the other things, just link to Epic.

## Building Epic

In order to do anything besides use pre-trained models, you will probably need to build Epic.

To build, you need a release of [SBT 0.13.2](http://www.scala-sbt.org/0.13.2/docs/Getting-Started/Setup.html)

then run 

<pre>
$ sbt assembly
</pre>

which will compile everything, run tests, and build a fatjar that includes all dependencies.


## Training Models

### Training Parsers

There are several different discriminative parsers you can train, and the trainer main class has lots of options. To get a sense of them, run the following command:
<pre>
$ java -cp target/scala-2.10/epic-assembly-0.2-SNAPSHOT.jar epic.parser.models.ParserTrainer --help
</pre>

You'll get a list of all the available options (so many!) The important ones are:

<pre>
--treebank.path "path/to/treebank"
--cache.path "constraint.cache"
--modelFactory  XXX                              # the kind of parser to train. See below.
--opt.useStochastic true                         # turn on stochastic gradient
--opt.regularization 1.0                         # regularization constant. you need to regularize, badly.
</pre>


There are 4 kinds of base models you can train, and you can tie them together with an `EPParserModel`, if you want. The 4 base models are:

  * epic.parser.models.LatentModelFactory: Latent annotation (like the Berkeley parser)
  * epic.parser.models.LexModelFactory: Lexical annotation (kind of like the Collins parser)
  * epic.parser.models.StructModelFactory: Structural annotation (kind of like the Stanford parser)
  * epic.parser.models.SpanModelFactory: Span features (Hall, Durrett, and Klein, 2014)
 

These models all have their own options. You can see those by specifying the modelFactory and adding --help: 
<pre>
$ java -cp target/scala-2.10/epic-assembly-0.2-SNAPSHOT.jar epic.parser.models.ParserTrainer --modelFactory "model" --help
</pre>

If you use the first three in research papers, please cite 

> David Hall and Dan Klein. 2012. Training Factored PCFGs with Expectation Propagation. In EMNLP.

If you use the `SpanModel`, please cite:

> David Hall, Greg Durrett, and Dan Klein. 2014. Less Grammar, More Features. In ACL.

If you use something else, cite one of these, or something.

For training a SpanModel, the following configurations are known to work well in general:


* English:
```bash
epic.parser.models.ParserTrainer \
  --modelFactory epic.parser.models.SpanModelFactory \
  --cache.path constraints.cache \
  --opt.useStochastic \
  --opt.regularization 5 \
  --opt.batchSize 500 \
  --alpha 0.1 \
  --maxIterations 1000 \
  --trainer.modelFactory.annotator epic.trees.annotations.PipelineAnnotator \
  --ann.0 epic.trees.annotations.FilterAnnotations \
  --ann.1 epic.trees.annotations.ForgetHeadTag \
  --ann.2 epic.trees.annotations.Markovize \
  --vertical 1 \
  --horizontal 0 \
  --treebank.path /home/dlwh/wsj/
```
* Other (SPMRL languages):
```bash
epic.parser.models.ParserTrainer \
  --treebankType spmrl \
  --binarization head \
  --modelFactory epic.parser.models.SpanModelFactory \
  --opt.useStochastic --opt.regularization 5.0 \
  --opt.batchSize 400 --maxIterations 502 \
  --iterationsPerEval 100 \
  --alpha 0.1 \
  --trainer.modelFactory.annotator epic.trees.annotations.PipelineAnnotator \
  --ann.0 epic.trees.annotations.FilterAnnotations  \
  --ann.1 epic.trees.annotations.ForgetHeadTag \
  --ann.2 epic.trees.annotations.Markovize \
  --ann.2.vertical 1 \
  --ann.2.horizontal 0 \
  --ann.3 epic.trees.annotations.SplitPunct \
  --cache.path $languc-constraints.cache \
  --treebank.path ${SPMRL}/${languc}_SPMRL/gold/ptb/ \
  --supervisedHeadFinderPtbPath ${SPMRL}/${languc}_SPMRL/gold/ptb/${train}/${train}.$lang.gold.ptb \
  --supervisedHeadFinderConllPath ${SPMRL}/${languc}_SPMRL/gold/conll/${train}/${train}.$lang.gold.conll \
  --threads 8 
```


Training a parser currently needs four files that are cached to the pwd:

* `xbar.gr`: caches the topology of the grammar
* `constraints.cache`, `constraints.cache.*`: remembers pruning masks computed from the base grammar.

TODO: remove this reliance.

#### Treebank types

There is a `treebank.type` commandline flag that supports a few different formats for treebanks. They are:
* `penn`: Reads from the `wsj/` subdirectory of the Penn Treebank. This expects a set of directories 00-24, each of which contains a number of `mrg` files. Standard splits are used.
* `chinese`: Expects a number of chtbNN.mrg files in a single directory.
* `negra`: Expects a directory with three files, `negra_[1-3].mrg`
* `conllonto`: Expects data formatted like the 2011 CoNLL shared task. Only reads the trees.
* `spmrl`: Expects a directory layout like that used in the 2012 SPMRL shared task.
* `simple`: Expects a directory with 3 files: `{train, dev, test}.txt`

##### Training a parser programmatically

You can also train a span model programmatically, by using the `SpanModelFactory.buildSimple` method. For example:

```scala
SpanModelFactory.buildSimple(trees, OptParams(regularization=1.0, useStochastic = true))
```

The build simple model also supports using custom featurizers.


#### Training POS taggers and other sequence models

The main class `epic.sequences.TrainPosTagger` can be used to train a POS Tagger from a treebank. It expects the same treebank options (namely `treebank.path` and `treebank.type`) as the Parser trainer does, as well as the same optimization options.


The following configuration is known to work well:

* English:
```bash
     epic.sequences.TrainPosTagger \
     --treebank.path $PATH_TO/wsj \
     --opt.regularization 2.0 \
     --useStochastic \
     --maxIterations 1000
```
* Others (SPMRL):
```bash
  epic.sequences.TrainPosTagger --opt.regularization 2.0 --useStochastic --maxIterations 1000 \
  --treebankType spmrl \
  --binarization left \
  --treebank.path ${SPMRL}/${languc}_SPMRL/gold/ptb/
```

If you want to train other kinds of models, you will probably need to build CRFs programmatically. For inspiration, you should probably look at the source code for TrainPosTagger. It's wonderfully short:

```scala
object TrainPosTagger extends LazyLogging {
  case class Params(opt: OptParams, treebank: ProcessedTreebank, hashFeatureScale: Double = 0.00)

  def main(args: Array[String]) {
    val params = CommandLineParser.readIn[Params](args)
    logger.info("Command line arguments for recovery:\n" + Configuration.fromObject(params).toCommandLineString)
    import params._
    val train = treebank.trainTrees.map(_.asTaggedSequence)
    val test = treebank.devTrees.map(_.asTaggedSequence)

    val crf = CRF.buildSimple(train, AnnotatedLabel("TOP"), opt = opt, hashFeatures = hashFeatureScale)

    val stats = TaggedSequenceEval.eval(crf, test)
    println("Final Stats: " + stats)
    println("Confusion Matrix:\n" + stats.confusion)

  }

}
```

Basically, you need to create a collection of TaggedSequences, which is a pair of sequences, one for tags and one for words. Then pass in the training data to `CRF.buildSimple`, along with a start symbol (used for the "beginning of sentence" tag), an optional [Gazetteer](#gazetteer) (not shown), and an [OptParams](#optparams), which is used to control the optimization. There is also an optional hashFeatures argument, which isn't used. 

We can also pass in two [`WordFeaturizer`] instances, one for "label" features, and one for "transition" features. Most of the featurizers in Epic have a cross product form `(Label x Surface)`, where `Label` is a feature on the label (e.g. the pos tag) and the `Surface` feature is a feature on the surface string.Here, the label featurizer features are crossed with the tag, and the transition featurizer features are crossed with pairs of sucessive labels. See the wiki page on [[Featurizers]] for more detail.


### Training NER systems and other segmentation models

Training an NER system or other SemiCRF is very similar to training a CRF. The main difference is that the inputs are Segmentations, rather than TaggedSequences. The main class `epic.sequences.SemiConllNERPipeline` can be used to train NER models, with data in the [CoNLL 2003 shared task format](http://www.cnts.ua.ac.be/conll2003/ner/). This class completely ignores all fields except the first and last. The commandline takes two paths, `--train` and `--test`, to specify training and test set files, respectively. 

If you need to do something more complicated, you will need to write your own code. As an example, here is the code for `epic.sequences.SemiConllNERPipeline`. This code is somewhat more complicated, as the CoNLL sequences need to be turned into segmentations.

```scala
def main(args: Array[String]) {
    val params:Params = CommandLineParser.readIn[Params](args)
    logger.info("Command line arguments for recovery:\n" + Configuration.fromObject(params).toCommandLineString)
    val (train,test) = {
      val standardTrain = CONLLSequenceReader.readTrain(new FileInputStream(params.path), params.path.getName).toIndexedSeq
      val standardTest = CONLLSequenceReader.readTrain(new FileInputStream(params.test), params.path.getName).toIndexedSeq

      standardTrain.take(params.nsents).map(makeSegmentation) -> standardTest.map(makeSegmentation)
    }


    // you can optionally pass in an a Gazetteer, though I've not gotten much mileage with them.
    val crf = SemiCRF.buildSimple(train, "--BEGIN--", "O", params.opt)

    val stats = SegmentationEval.eval(crf, test)

    println(stats)


  }
```

We can also pass in featurizers, like in the CRF trainer. In this case, we can pass in a [`WordFeaturizer`] and a [`SpanFeaturizer`]. WordFeatures are like before, while SpanFeaturizer give features over the entire input span. For NER, this can be useful for adding features noting that an entity is entirely surrounded by quotation marks, for instance, or for matching against entries in a [[Gazetteer]].

### OptParams

OptParams is a configuration class that controls the optimizer. There are a bunch of different options:
```scala
--opt.batchSize: Int = 512                                                                                                                                                                                                                 
--opt.regularization: Double = 0.0                                                                                                                                                                                                         
--opt.alpha: Double = 0.5                                                                                                                                                                                                                  
--opt.maxIterations: Int = 1000                                                                                                                                                                                                            
--opt.useL1: Boolean = false                                                                                                                                                                                                               
--opt.tolerance: Double = 1.0E-5                                                                                                                                                                                                           
--opt.useStochastic: Boolean = false                                                                                                                                                                                                       
--opt.randomSeed: Int = 0     
```

Regularization is generally very important. Using a value of 1.0 usually works pretty well. 5.0 works better on the SpanModel for parsing. `useStochastic` turns on stochastic gradient descent (rather than full batch optimization). It makes training much faster, usually.

