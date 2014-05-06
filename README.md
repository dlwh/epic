### ReadMe is Work in Progress.

# Epic

(c) 2014- David Hall.

Epic is a structured prediction framework for Scala. It also includes classes for training high-accuracy syntactic parsers, part-of-speech tagging systems, name entity recognizers, and more

The current version is 1.0-SNAPSHOT.

## Documentation

Documentation will live at the GitHub wiki: <https://github.com/dlwh/epic/wiki>

## Using Epic

### Command-line Usage

#### Parser

#### Part-of-Speech Tagger

#### Named Entity Recognition

### Programmatic Usage

#### Parser

#### Part-of-Speech Tagger

#### Named Entity Recognition

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

If you use any of these models, please cite:

> David Hall, Greg Durrett, and Dan Klein. 2014. Less Grammar, More Features. In ACL.


XXX

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




