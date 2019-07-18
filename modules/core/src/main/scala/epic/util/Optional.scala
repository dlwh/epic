package epic.util

/**
 * Optional is a wrapper of [[scala.Option]] with an implicit conversion from Any to Optional.
 * Mostly for use in optional argument lists.
 * @author dlwh, jovilius
 */
sealed trait Optional[+A] {
  def foldLeft[B](b: B)(f: (B, A) => B) = toOption.foldLeft(b)(f)

  def toOption = this match {
    case NotProvided => None
    case Provided(v) => Some(v)
  }
}
case class Provided[A](value: A) extends Optional[A]
@SerialVersionUID(-649101350749082174L)
case object NotProvided extends Optional[Nothing]

object Optional {
  implicit def anyToOptional[A](x: A): Optional[A] = if (x == null) NotProvided else Provided(x)
  implicit def optionToOptional[A](x: Option[A]): Optional[A] = x.fold(NotProvided:Optional[A])(Provided(_))
  implicit def optionalToOption[A](x: Optional[A]): Option[A] = x.toOption
}
