package scalanlp.trees;

case class Span(st: Int, e: Int) extends Range(st,e,1)  {
  require(start <= end);

  /**
  * return true if the ranges have a non-empty intersection
  */
  def crosses(other: Span) = {
    start == other.start || end == other.end ||
      ( start < other.start &&  end > other.start ) ||
      ( start > other.start && other.end > start)
  }

  /**
  * Return true if this' range contains the other range.
  */
  def contains(other:Span) = {
    start <= other.start && end >= other.end;
  }

  override def toString = "Span("+start + "," + end + ")";

}
