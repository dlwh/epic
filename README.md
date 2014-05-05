### ReadMe is Work in Progress.

# Epic

(c) 2014- David Hall.

Epic is a structured prediction framework for Scala. It also includes classes for training high-accuracy syntactic parsers, part-of-speech tagging systems, name entity recognizers, and more

The current version is 1.0-SNAPSHOT.

## Documentation

Documentation will live at the GitHub wiki: <https://github.com/dlwh/epic/wiki>

## Using Epic

### Pre-trained Models

Epic provides a number of pretrained models. These are available as Maven artifacts, and can be loaded at runtime.

## Pre-trained Parsers

* English: TODO
  ```scala
  "org.scalanlp" %% "epic-parser-en-span" % "1.0-SNAPSHOT"
  ```
* Basque: TODO
  ```scala
  "org.scalanlp" %% "epic-parser-eu-span" % "1.0-SNAPSHOT"
  ```
* French: TODO
  ```scala
  "org.scalanlp" %% "epic-parser-fr-span" % "1.0-SNAPSHOT"
  ```
* German: TODO
  ```scala
  "org.scalanlp" %% "epic-parser-de-span" % "1.0-SNAPSHOT"
  ```
* Hungarian: TODO
  ```scala
  "org.scalanlp" %% "epic-parser-pl-span" % "1.0-SNAPSHOT"
  ```
* Korean: TODO
  ```scala
  "org.scalanlp" %% "epic-parser-ko-span" % "1.0-SNAPSHOT"
  ```
* Polish: TODO
  ```scala
  "org.scalanlp" %% "epic-parser-ko-span" % "1.0-SNAPSHOT"
  ```
* Swedish: TODO
  ```scala
  "org.scalanlp" %% "epic-parser-sv-span" % "1.0-SNAPSHOT"
  ```




### Building

To build, you need a release of [SBT 0.13.2](http://www.scala-sbt.org/0.13.2/docs/Getting-Started/Setup.html)

then run 

<pre>
$ sbt assembly
</pre>

which will compile everything, run tests, and build a jar.

## Training Parsers

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


