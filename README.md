### README is Work in Progress.

# Epic

(c) 2014- David Hall.

Epic is a structured prediction framework for Scala. It also includes classes for training high-accuracy syntactic parsers, part-of-speech taggers, name entity recognizers, and more.

The current version is 1.0-SNAPSHOT.

## Documentation

Documentation will live at the GitHub wiki: <https://github.com/dlwh/epic/wiki>

## Using Epic

### Command-line Usage

Epic bundles command line interfaces for using parsers, NER systems, and POS taggers (and more generally, segmentation and tagging systems). There are three classes, one for each kind of system:

* `epic.parser.ParseText` runs a parser.
* `epic.sequences.SegmentText` runs an NER system, or any kind of segmentation system.
* `epic.sequences.TagText` runs a POS tagger, or any kind of tagging system.

All of these systems expect plain text files as input, along with a path to a model file. The syntax is:

```bash
java -Xmx4g -cp /path/to/epic-assembly-1.0-SNAPSHOT.jar epic.parser.ParseText --model /path/to/model.ser.gz --nthreads <number of threads> [files]
```

Currently, all text is output to standard out. In the future, we will support output in a way that differentiates the files. In addition, we will add support for reading from stdin. By default, the system will use all available cores for execution.

TODO: Models can be downloaded from <https://www.scalanlp.org/epic-models/> or from Maven Central. ([See below](#pre-trained-models).)

### Programmatic Usage

Epic also supports programmatic usage. All of the models assume that text has been segmented and tokenized.

#### Preprocessing text

To preprocess text so that the models can use them, you will need to segment out sentences and tokenize the sentences into individual words. Epic comes with classes to do both.

TODO: bundle the sentence segmenter

To split a text into sentences, use the XXX.

Once you have a sentence, you can tokenize it using a `epic.preprocess.TreebankTokenizer`, which takes a string and returns a sequence of tokens. All told, the pipeline looks like this:

```scala
val text = getSomeText();

val sentenceSplitter = ??? // TODO
val tokenizer = new epic.preprocess.TreebankTokenizer()

val sentences: IndexedSeq[IndexedSeq[String]] = sentenceSplitter(text).map(tokenizer).toIndexedSeq

for(sentence <- sentences) {
  // use the sentence tokens
}

```



#### Parser

TODO: epic.models.deserialize

To use the parser programmaticaly, deserialize a parser model--either using `epic.models.deserialize[Parser[AnnotatedLabel, String]](path)` or using the ParserSelector. Then, give the parser segmented and tokenized text:

```scala
val parser = epic.models.deserialize[Parser[AnnotataedLabel, String]](path)

// or:

val parser = epic.models.ParserSelector.loadParser("en") // or another 2 letter code.

val tree = parser(sentence)

println(tree.render(words))

```

Trees have a number of methods on them. See the class definition or API docs (TODO: publish api docs)

#### Part-of-Speech Tagger

Using a Part-of-Speech tagger is similar to using a parser: load a model, tokenize some text, run the tagger. All taggers are (currently) [linear chain conditional random fields](http://people.cs.umass.edu/~mccallum/papers/crf-tutorial.pdf), or CRFs. (You don't need to understand them to use them. They are just a machine learning method for assigning a sequence of tags to a sequence of words.)

```scala
val tagger = epic.models.deserialize[CRF[AnnotatedLabel, String]](path)

// or:

val tagger = epic.models.PosTaggerSelector.loadTagger("en") // or another 2 letter code.

val tags = tagger(sentence)

println(tags.render)

```


#### Named Entity Recognition

Again, using NER systems is basically the same as using POS taggers. All NER systems are currently [semi-Markov linear chain conditional random fields](http://www.cs.cmu.edu/~wcohen/postscript/semiCRF.pdf), or SemiCRFs. (You don't need to understand them to use them. They are just a machine learning method for segmenting a sequence of words into continguous segments.)

```scala
val tagger = epic.models.deserialize[SemiCRF[AnnotatedLabel, String]](path)

// or:

val tagger = epic.models.NerSelector.loadSegmenter("en") // or another 2 letter code.

val segments = tagger(sentence)

println(tags.render(tagger.outsideLabel))

```

The outside label of a SemiCRF is the label that is consider not part of a "real" segment. For instance, in NER, it is the label given to words that are not named entities.

### Pre-trained Models

Epic provides a number of pretrained models. These are available as Maven artifacts from Maven Central, and can be loaded at runtime. To use a specific model, just depend on it (or alternatively download the jar file). You can then load the parser by calling, for example:

```scala
epic.parser.models.en.span.EnglishSpanParser.load()
```

This will load the  model and return a `Parser` object. If you want to not hardwire dependencies, either for internationalization or to potentially try different models, use `epic.models.ParserSelector.loadParser(language)`, where
language is the [two letter code for the language](http://www.loc.gov/standards/iso639-2/php/code_list.php) you want to use.

To following models are available at this time:


* Parser
  * English: TODO
    ```
    "org.scalanlp" %% "epic-parser-en-span" % "1.0-SNAPSHOT"
    ```
  * Basque: TODO
    ```
    "org.scalanlp" %% "epic-parser-eu-span" % "1.0-SNAPSHOT"
    ```
  * French: TODO
    ```
    "org.scalanlp" %% "epic-parser-fr-span" % "1.0-SNAPSHOT"
    ```
  * German: TODO
    ```
    "org.scalanlp" %% "epic-parser-de-span" % "1.0-SNAPSHOT"
    ```
  * Hungarian: TODO
    ```
    "org.scalanlp" %% "epic-parser-pl-span" % "1.0-SNAPSHOT"
    ```
  * Korean: TODO
    ```
    "org.scalanlp" %% "epic-parser-ko-span" % "1.0-SNAPSHOT"
    ```
  * Polish: TODO
    ```
    "org.scalanlp" %% "epic-parser-ko-span" % "1.0-SNAPSHOT"
    ```
  * Swedish: TODO
    ```
    "org.scalanlp" %% "epic-parser-sv-span" % "1.0-SNAPSHOT"
    ```
* POS Taggers
  * English: TODO
    ```
    "org.scalanlp" %% "epic-pos-en" % "1.0-SNAPSHOT"
    ```
  * Basque: TODO
    ```
    "org.scalanlp" %% "epic-pos-eu" % "1.0-SNAPSHOT"
    ```
  * French: TODO
    ```
    "org.scalanlp" %% "epic-pos-fr" % "1.0-SNAPSHOT"
    ```
  * German: TODO
    ```
    "org.scalanlp" %% "epic-pos-de" % "1.0-SNAPSHOT"
    ```
  * Hungarian: TODO
    ```
    "org.scalanlp" %% "epic-pos-pl" % "1.0-SNAPSHOT"
    ```
  * Korean: TODO
    ```
    "org.scalanlp" %% "epic-pos-ko" % "1.0-SNAPSHOT"
    ```
  * Polish: TODO
    ```
    "org.scalanlp" %% "epic-pos-ko" % "1.0-SNAPSHOT"
    ```
  * Swedish: TODO
    ```
    "org.scalanlp" %% "epic-pos-sv" % "1.0-SNAPSHOT"
    ```
* Named Entity Recognizers
  * English: TODO
    ```
    "org.scalanlp" %% "epic-ner-en-conll" % "1.0-SNAPSHOT"
    ```
  * Spanish: TODO
    ```
    "org.scalanlp" %% "epic-ner-es-conll" % "1.0-SNAPSHOT"
    ```
  * German: TODO
    ```
    "org.scalanlp" %% "epic-ner-de-conll" % "1.0-SNAPSHOT"
    ```


If you use any of the parser models in research publications, please cite:

> David Hall, Greg Durrett, and Dan Klein. 2014. Less Grammar, More Features. In ACL.

If you use the other things, just link to Epic.


## Training Models

### Training Parsers

There are several different discriminative parsers you can train, and the trainer main class has lots of options. To get a sense of them, run the following command:
<pre>
$ java -cp target/epic-assembly-0.1-SNAPSHOT.jar epic.parser.models.ParserTrainer --help
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
$ java -cp target/epicparser-assembly-0.1.jar epic.parser.models.ParserPipeline --modelFactory "model" --help
</pre>

If you use the first three in research papers, please cite 

> David Hall and Dan Klein. 2012. Training Factored PCFGs with Expectation Propagation. In EMNLP.

If you use the `SpanModel`, please cite:

> David Hall, Greg Durrett, and Dan Klein. 2014. Less Grammar, More Features. In ACL.

If you use something else, cite one of these, or something.

#### Treebank types

There is a `treebank.type` commandline flag that supports a few different formats for treebanks. They are:
* `penn`: Reads from the `wsj/` subdirectory of the Penn Treebank. This expects a set of directories 00-24, each of which contains a number of `mrg` files. Standard splits are used.
* `chinese`: Expects a number of chtbNN.mrg files in a single directory.
* `negra`: Expects a directory with three files, `negra_[1-3].mrg`
* `conllonto`: Expects data formatted like the 2011 CoNLL shared task. Only reads the trees.
* `spmrl`: Expects a directory layout like that used in the 2012 SPMRL shared task.
* `simple`: Expects a directory with 3 files: `{train, dev, test}.txt`

##### Training a parser programmatically

TODO



## Building Epic

To build, you need a release of [SBT 0.13.2](http://www.scala-sbt.org/0.13.2/docs/Getting-Started/Setup.html)

then run 

<pre>
$ sbt assembly
</pre>

which will compile everything, run tests, and build a jar.




