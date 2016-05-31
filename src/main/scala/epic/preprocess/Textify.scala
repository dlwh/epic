package epic.preprocess

import java.io.{PrintWriter, FileWriter, File}

/**
 * TODO
 *
 * @author dlwh
 **/
object Textify {

  def main(args: Array[String]): Unit= {

    val outdir = new File(args(0))
    outdir.mkdirs()

    for(x <- args.drop(1); dir = new File(x); f <- if (dir.isDirectory) dir.listFiles() else Array(dir)) {
      println(f)
      val out = new File(outdir, f.getName)
      val toks = preprocess(f)
      val oo = new PrintWriter(new FileWriter(out))
      for(line <- toks) {
        oo.println(line.mkString("\t"))
      }
      oo.close()
    }
  }

}
