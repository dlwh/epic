# Chalk

## Introduction

Chalk is a library for natural language processing (NLP).


### What's inside

The latest stable version is 1.3.0. Changes from the previous release include:

* Initial implementation of functional pipelines for NLP applications, and actor-based pipelines based on those components.

See the [CHANGELOG](https://github.com/scalanlp/chalk/wiki/CHANGELOG) for changes in previous versions.

## Using Chalk


In SBT:

    libraryDependencies += "org.scalanlp" % "chalk" % "1.3.0"

In Maven:

    <dependency>
       <groupId>org.scalanlp</groupId>
       <artifactId>chalk</artifactId>
       <version>1.3.0</version>
    </dependency>

## Requirements

* Version 1.6 of the Java 2 SDK (http://java.sun.com)

## Configuring your environment variables

Set `JAVA_HOME` to match the top level directory containing the Java installation you want to use. If you want to be able to use the `chalk` command line executable, set `CHALK_DIR` to where you put Chalk, and then add the directory `CHALK_DIR/bin` to your path.


## Building the system from source

Chalk uses SBT (Simple Build Tool) with a standard directory structure.  To build Chalk, type (in the `CHALK_DIR` directory):

	$ ./build update compile

This will compile the source files and put them in `./target/classes`. If this is your first time running it, you will see messages about Scala being downloaded -- this is fine and expected. Once that is over, the Chalk code will be compiled.

To try out other build targets, do:

	$ ./build

To make sure all the tests pass, do:

	$ ./build test

Documentation for SBT is at <http://www.scala-sbt.org/>


# Questions or suggestions?

Email Jason Baldridge: <jasonbaldridge@gmail.com>

Or, create an issue: <https://github.com/scalanlp/chalk/issues>
