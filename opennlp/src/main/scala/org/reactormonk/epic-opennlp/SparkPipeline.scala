package org.reactormonk.epic.opennlp

import epic.slab._
import epic.slab.Utils._
import java.net.URL
import opennlp.tools.sentdetect.SentenceModel
import opennlp.tools.tokenize.TokenizerModel
import opennlp.tools.namefind.TokenNameFinderModel
import opennlp.tools.chunker.ChunkerModel
import opennlp.tools.postag.POSModel
import opennlp.tools.parser.ParserModel
import org.apache.spark.SparkContext
import org.apache.spark.SparkConf
import org.apache.spark.rdd.RDD

object SparkPipeline {
  def resource(name: String): URL = {
    getClass.getResource("/org/reactormonk/epic-opennlp/opennlp-models/" + name)
  }
  val sentence = SentenceDetector(new SentenceModel(resource("en-sent.bin")))
  val tokenizer = Tokenizer(new TokenizerModel(resource("en-token.bin")))
  val person = Tagger.person(new TokenNameFinderModel(resource("en-ner-person.bin")))
  val organization = Tagger.organization(new TokenNameFinderModel(resource("en-ner-organization.bin")))
  val money = Tagger.money(new TokenNameFinderModel(resource("en-ner-money.bin")))
  val date = Tagger.date(new TokenNameFinderModel(resource("en-ner-date.bin")))
  val location = Tagger.location(new TokenNameFinderModel(resource("en-ner-location.bin")))
  val percentage = Tagger.percentage(new TokenNameFinderModel(resource("en-ner-percentage.bin")))
  val time = Tagger.time(new TokenNameFinderModel(resource("en-ner-time.bin")))
  val pos = PosTagger(new POSModel(resource("en-pos-maxent.bin")))
  val chunker = Chunker(new ChunkerModel(resource("en-chunker.bin")))
  val parser = Parser(new ParserModel(resource("en-parser-chunking.bin")))

  lazy val sc = {
    val conf = new SparkConf()
      .setAppName("OpenNLP Pipeline")
      .set("spark.serializer", "org.apache.spark.serializer.KryoSerializer")
    new SparkContext(conf)
  }

  def pipeline(files: RDD[String]) = {
    files
      .map(sentence.slabFrom(_))
      .map(tokenizer(_))
      .map(person(_))
      .map(organization(_))
      .map(money(_))
      .map(date(_))
      .map(location(_))
      .map(percentage(_))
      .map(time(_))
      .map(pos(_))
      .map(chunker(_))
      .map(parser(_))
  }

  def readFiles(path: String): Array[String] = {
    new java.io.File(path).listFiles.map(f => scala.io.Source.fromFile(f).mkString)
  }
}

object WithRepartition {
  import SparkPipeline._
  def main(args: Array[String]) {
    val files = sc.parallelize(readFiles(args(0)), Integer.parseInt(args(1)))
    pipeline(files).foreach(print)
  }
}

object WithoutRepartition {
  import SparkPipeline._
  def main(args: Array[String]) {
    val files = sc.parallelize(readFiles(args(0)))
    pipeline(files).foreach(print)
  }
}
