# Slabs

Slabs is a data structure to store and retrieve annotations on a
document. It is based on HLists from
[shapeless](https://github.com/milessabin/shapeless).

Slabs is distributed under the [Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0.html).

The current version is 0.1-SNAPSHOT.

## Documentation

There isn't any direct documentation available. If you want to write
annotators which use slabs, take a look at the example annotators and
the opennlp wrappers. The annotations themselves are not stored in any
ordered fashion. If you require ordering, please take a look at the
indexes.

## Hacking

The basic structure of a slab is the annotations, which contains an
HList of all the annotations. The annotations themselves are stored in
Lists. Using a collection interface instead doesn't work, so there's
only Lists.
