package epic.framework

trait ExpectedCounts[Self <: ExpectedCounts[Self]] { this: Self =>
  def +=(other: Self): Self

  def -=(other: Self): Self

  def loss: Double
}
