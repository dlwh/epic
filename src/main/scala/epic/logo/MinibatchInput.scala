package epic.logo

case class MinibatchInput[T, W](val instance : Instance[T, W], val instanceNum : Int)

case class MinibatchOutput[T, W](val instance : Instance[T, W], val instanceNum : Int, val df : W, val loss : Double)
