To build, you need a release of [SBT 0.11.2](https://github.com/harrah/xsbt/wiki/Getting-Started-Setup)

then run 

<pre>
$ sbt assembly
</pre>

which will compile everything, run tests, and build a jar.

To build a discriminative parser, several steps are needed. First, you need to build a baseline generative parser.
<pre>
$ java -Xmx4g -cp target/epicparser-assembly-0.1.jar epic.parser.GenerativeTrainer --treebank.path /path/to/treebank --baseParser.path xbar.gr
</pre>

Second, you need to generate pruning masks for all trees in the treebank using this parser:
<pre>
$ java -Xmx6g -cp target/epicparser-assembly-0.1.jar epic.parser.projections.ProjectTreebankToConstraints  --treebank.path /path/to/treebank --parser parsers/Gen.parser
</pre>

Now you can actually train a parser! You have many options. To get a sense of them, run the following command:
<pre>
$ java -cp target/epicparser-assembly-0.1.jar epic.parser.models.ParserPipeline --help
</pre>

You'll get a list of all the available options (so many!) The important ones are:

<pre>
--treebank.path "path/to/treebank"
--constraints.path "path/to/constraints.ser.gz"
--baseParser.path "path/to/xbar.gr"
--modelFactory                                   # the kind of parser to train. See below.
--opt.useStochastic true                         # turn on stochastic gradient
</pre>


There are 4 kinds of base models you can train, and you can tie them together with an `EPParserModel`, if you want. The 4 base models are:

  * epic.parser.models.LatentModelFactory: Latent annotation (like the Berkeley parser)
  * epic.parser.models.LexModelFactory: Lexical annotation (kind of like the collins parser)
  * epic.parser.models.StructModelFactory: Structural annotation (the Stanford parser)
  * epic.parser.models.SpanModelFactory: Span features (Petrov 2008 or Finkel 2008, etc.)

These models all have their own options. You can see those by specifying the modelFactory and adding --help: 
<pre>
$ java -cp target/epicparser-assembly-0.1.jar epic.parser.models.ParserPipeline --modelFactory "model" --help
</pre>

None of these models are good by themselves: you need to train them jointly. To do that, use epic.models.EPParserModelFactory:
<pre>
$ java -cp target/epicparser-assembly-0.1.jar epic.parser.models.ParserPipeline --modelFactory epic.models.EPParserModelFactory --model.0 "model the first" --model.1 "model the second" // etc.
</pre>
