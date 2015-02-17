# Slabs

Slabs is a data structure to store and retrieve annotations on a
document. It is based on HLists from
[shapeless](https://github.com/milessabin/shapeless).

Slabs is distributed under the [Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0.html).

## Pipelines

### Building a Pipeline

Each pipeline consists of annotators, which are functions that
transform a slab by adding more annotations. The annotators are
usually created by passing a library constructor a corresponding
model.

    val sentenceSegmenter = library.SentenceSegmenter(sentenceModel)
    val tokenizer = library.Tokenizer(tokenModel)

The pipeline starts with wrapping your documents into Slabs.

    val slabs = documents.map(Slab.apply)

Then you apply the different annotators in order.

    val tokenized = slabs.map(sentenceSegmenter(_)).map(tokenizer(_))

The `(_)` syntax is required because a sentenceSegmenter can't
actually implement a Function interface.

To add more annotations, create more annotators from the library.

    val ner = library.NERAnnotator(nerModel)
    val date = library.DateAnnotator(dateModel)

    val annotated = tokenizer.map(ner(_)).map(date(_))

Because the two annotators are independent, the order doesn't matter.

    val annotated = tokenizer.map(date(_)).map(ner(_))

produces the exact same result.

### Extracting Data

Shapeless hlists rely a lot on implicits and handle poorly when
removed from their implicit scope. To store the annotations in a more
stable data type, you can convert the annotations to a tuple:

    import shapeless._
    val annotations = slab.annotations.tupled

which gives you a tuple corresponding to the current annotations.
Naturally, this version isn't stable against edits in the pipeline. To
always get the same tuple back, select the items you want beforehand:

    import shapeless._
    val annotations = slab.
      selectMany[List[Sentence] :: List[Token] :: HNil].tupled

With the selectMany, the tuple is guaranteed to be
`Tuple2[List[Sentence], List[Token]`. The order of the elements in the
annotation hlist doesn't matter, it is possible to reorder the
elements for the tuple.

    import shapeless._
    val annotations = slab.
      selectMany[List[Token] :: List[Sentence] :: HNil].tupled


### Debugging a Pipeline

With shapeless 2.1.0, you can attach a type trace to the annotations.

    def typeTrace[T](t: => T)(implicit tt: TypeTrace[T]): Unit = ()
    typeTrace(slab.annotations)

## Hacking

The basic structure of a slab is the annotations, which contains an
HList of all the annotations. The annotations themselves are stored in
Lists. Using a collection interface instead doesn't work, so there's
only Lists.
