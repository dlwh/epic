package epic.everything

import java.io.{FileWriter, PrintWriter, File}
import io.Source

case class IntermediateCache(cacheDir: File = new File("cache"),
                             clearCache: Boolean = false,
                             dropComponents: String = "",
                             enforceChecksum: Boolean = true) {
  if(clearCache && cacheDir.exists()) {
    deleteRecursive(cacheDir)
  }

  cacheDir.mkdirs()

  val componentsToDrop = dropComponents.split(",").toSet

  def cached[T](name: String, dependencies: Any*)(body: =>T):T = {
    val f = new File(cacheDir, name)
    val checksumFile = new File(cacheDir, name+".checksums")
    val depHashCodes = dependencies.map(_.hashCode)
    val readIn = if(f.exists && checksumFile.exists) {
      var ok = !componentsToDrop(name)
      if(ok && enforceChecksum) {
        val codes = Source.fromFile(checksumFile).getLines().toIndexedSeq
        ok = codes.length == dependencies.length
        if(!ok)
          println(s"CACHE: Rebuilding $name because we have ${codes.length} dependencies in cache and ${dependencies.length} in this run.")
        else {
          for( ((cached, cur),code) <- codes zip dependencies zip depHashCodes if ok) {
            if(cached.toLong != code) {
              println(s"CACHE: Rebuilding $name because ${cur.toString.take(10)}'s hashcode has changed: ${cached.trim} vs. $code")
              ok = false
            }
          }
        }
      }
      ok
    } else {
      false
    }

    if(readIn) {
      breeze.util.readObject[T](f)
    } else {
      val x = body
      val out = new PrintWriter(new FileWriter(checksumFile))
      depHashCodes foreach {out.println(_)}
      out.close()
      breeze.util.writeObject(f, x)
      x
    }
  }

  private def deleteRecursive(path: File):Boolean = {
    if (!path.exists()) return true
    var ret = true
    if (path.isDirectory){
      for (f <- path.listFiles){
        ret = ret && deleteRecursive(f)
      }
    }
    ret && path.delete()
  }

}