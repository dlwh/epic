package epic.trees.util

import java.io.{PrintWriter, File}

import breeze.config.CommandLineParser
import epic.trees.PennTreeReader

/**
 * TODO
 *
 * @author dlwh
 **/
object FilterTreesByLength {

  case class Params(gold: File, guess: File, out: File, ignorePunct: Boolean = false, bucketSize: Int = 5)

  def main(args: Array[String]) = {
    val params = CommandLineParser.readIn[Params](args)
    import params._

    out.mkdirs()

    val goldOut = new collection.mutable.HashMap[Int, PrintWriter] {
      override def default(key: Int): PrintWriter = {
        new File(out, key.toString).mkdirs()
        val res = new PrintWriter(new File(out, s"/$key/gold"))
        update(key, res)
        res
      }
    }

    val guessOut = new collection.mutable.HashMap[Int, PrintWriter] {
      override def default(key: Int): PrintWriter = {
        new File(out, key.toString).mkdirs()
        val res = new PrintWriter(new File(out, s"/$key/guess"))
        update(key, res)
        res
      }
    }

    for ( (gold, guess) <- getTrees(gold) zip getTrees(params.guess)) {
      assert(gold._2 == guess._2)
      val len = (if (ignorePunct) gold._2.count(!_.forall(!_.isLetterOrDigit)) else gold._2.length)/bucketSize * bucketSize
      goldOut(len).println(gold._1.render(gold._2, newline = false))
      guessOut(len).println(guess._1.render(guess._2, newline = false))
    }

    goldOut.values.foreach(_.close())
    guessOut.values.foreach(_.close())

  }

  def getTrees(file: File): PennTreeReader = {
    new PennTreeReader(new java.io.FileReader(file))
  }
}
