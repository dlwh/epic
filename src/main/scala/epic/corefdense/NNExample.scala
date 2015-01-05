package epic.corefdense

trait NNExample[T] extends LabeledObject[T] {
  
  def input: Array[Double];
  
  def getLabel: T; 
  
  override def toString() = {
    "NNExample(" + input.toSeq + ", " + getLabel + ")"
  }
}

case class NNExampleImpl[T](val myInput: Array[Double], val myLabel: T) extends NNExample[T] {
  override def input = myInput;
  override def getLabel = myLabel;
}