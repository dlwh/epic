package scalanlp

import trees.Rule

/**
 *
 * @author dlwh
 */
package object parser {
  // tags for index types
  trait Ref[T]
  type RuleRef[L] = Ref[Rule[L]]

}
