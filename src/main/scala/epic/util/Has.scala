package epic.util

import epic.framework.{Example, Observation}

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
  implicit def identityHas2[H]:Has2[H, H] = new Has2[H, H] with Serializable  {
    def get(h: H): H = h
  }

  implicit def pairHas2_1[H,X]:Has2[(H,X), H] = new Has2[(H, X), H]  with Serializable {
    def get(h: (H, X)): H = h._1
  }

  implicit def pairHas2_2[H,X]:Has2[(H,X), X] = new Has2[(H, X), X]  with Serializable {
    def get(h: (H, X)): X = h._2
  }

  implicit def featuresOfObservation[X,F](implicit xx: X<:<Observation[F]): Has2[X, F] = new Has2[X, F]  with Serializable {
    def get(h: X): F = h.features
  }

  implicit def labelOfExample[X,L,L2,F](implicit xx: X<:<Example[L2, F], hh: Has2[L2, L]): Has2[X, L] = new Has2[X, L] with Serializable {
    def get(h: X): L = hh.get(h.label)
  }
}

