package epic.util

/**
 *
 * @author dlwh
 */
trait Has[WhatIHave] {
  type R[Haver] = Has2[Haver, WhatIHave]
  def apply[H](haver: H)(implicit h: R[H]) = h.get(haver)
}

trait Has2[Haver, +WhatIHave] {
    def get(h: Haver):WhatIHave
}


object Has2 {
  implicit def identityHas2[H]:Has2[H, H] = new Has2[H, H] {
    def get(h: H): H = h
  }

  implicit def pairHas2_1[H,X]:Has2[(H,X), H] = new Has2[(H, X), H] {
    def get(h: (H, X)): H = h._1
  }

  implicit def pairHas2_2[H,X]:Has2[(H,X), X] = new Has2[(H, X), X] {
    def get(h: (H, X)): X = h._2
  }
}

