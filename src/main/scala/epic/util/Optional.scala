package epic.util

/**
 * Optional is a wrapper of [[scala.Option]] with an implicit conversion from Any to Optional.
 * Mostly for use in optional argument lists.
 * @author dlwh, jovilius
 */
case class Optional[+A](value: Option[A]) {
  def foldLeft[B](b: B)(f: (B, A) => B) = value.fold(b)(x => f(b, x))
}

object Optional {

  implicit def anyToOptional[A](x: A): Optional[A] = if (x == null) Optional(None) else Optional(Some(x))

  implicit def optionToOptional[A](x: Option[A]): Optional[A] = Optional(x)

  implicit def optionalToOption[A](x: Optional[A]): Option[A] = x.value
  
}
