# Chalk

## Introduction

Chalk is a library for natural language processing (NLP). It was based on the [OpenNLP](http://opennlp.apache.org/) library, but is now transitioning to incorporate breeze.process instead. In fact, this version (1.1.3) is just a shell that is ready to receive code from breeze.process. There won't even be a Maven dependency published.

The name Chalk comes from the name given to one of Jason's son's stuffed elephants. "Chalk" obviously has a great connection to writing (and thus text processing). Of course, it also follows the what-your-kid-called-his-elephant naming convention precedent established with Hadoop. :)

### What's inside

The latest stable release is 1.1.3. What's new for this version?

* All OpenNLP Tools code removed.
* Add Twokenize (from Scalabha).

See the [CHANGELOG](https://github.com/scalanlp/chalk/wiki/CHANGELOG) for changes in previous versions.

## Using Chalk

As no dependency is being made for 1.1.3, you are welcome to try out 1.1.2 and check back later for a completely different library.

In SBT:

    libraryDependencies += "org.scalanlp" % "chalk" % "1.1.2"

In Maven:

    <dependency>
       <groupId>org.scalanlp</groupId>
       <artifactId>chalk</artifactId>
       <version>1.1.2</version>
    </dependency>

## Requirements

* Version 1.6 of the Java 2 SDK (http://java.sun.com)

## Configuring your environment variables

The easiest thing to do is to set the environment variables `JAVA_HOME`
and `CHALK_DIR` to the relevant locations on your system. Set `JAVA_HOME`
to match the top level directory containing the Java installation you
want to use.

Next, add the directory `CHALK_DIR/bin` to your path. For example, you
can set the path in your `.bashrc` file as follows:

	export PATH=$PATH:$CHALK_DIR/bin

Once you have taken care of these three things, you should be able to
build and use Chalk.


## Building the system from source

Chalk uses SBT (Simple Build Tool) with a standard directory
structure.  To build Chalk, type (in the `CHALK_DIR` directory):

	$ ./build update compile

This will compile the source files and put them in
`./target/classes`. If this is your first time running it, you will see
messages about Scala being downloaded -- this is fine and
expected. Once that is over, the Chalk code will be compiled.

To try out other build targets, do:

	$ ./build

This will drop you into the SBT interface. To see the actions that are
possible, hit the TAB key. (In general, you can do auto-completion on
any command prefix in SBT, hurrah!)

To make sure all the tests pass, do:

	$ ./build test

Documentation for SBT is at <http://www.scala-sbt.org/>

Note: if you have SBT already installed on your system, you can
also just call it directly with "sbt" in `CHALK_DIR`.


## Trying it out

Assuming you have completed all of the above steps, including running the "compile" action in SBT, you should now be able to try out some examples. Check out the [Chalk tutorials](Tutorial) for examples.


# Questions or suggestions?

Email Jason Baldridge: <jasonbaldridge@gmail.com>

Or, create an issue: <https://github.com/scalanlp/chalk/issues>
