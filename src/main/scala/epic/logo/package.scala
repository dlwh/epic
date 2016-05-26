package epic

package object logo {

  def clip(d : Double, l : Double, u : Double) = {
    (d max l) min u
  }

  type LabeledDatum[L, W] = epic.framework.Example[L, W]
}
