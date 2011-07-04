package scalanlp.parser

/*
 Copyright 2010 David Hall

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/



import scalanlp.util.Encoder;
import scalanlp.collection.mutable.SparseArrayMap;
import scalanlp.util.Index
import scalanlp.tensor.sparse.OldSparseVector
import scalala.tensor.{Counter2,::}

sealed trait Rule[@specialized(Int) +L] { def parent: L; def children: Seq[L] }





/*
object Grammar {
  import scalanlp.serialization.DataSerialization._;
  import scalanlp.serialization.DataSerialization
  implicit def grammarIsWritable[L:Writable]: Writable[Grammar[L]] = new Writable[Grammar[L]] {
    def write(out: DataOutput, g: Grammar[L]) = {
      // Grammar consists of an index, unary rules, and binary rules.
      DataSerialization.write(out, g.index);
      // Unary format: (parent index,<sparse vec of unary rules>)*, -1
      for(i <- 0 until g.index.size) {
        val vec = g.unaryRulesByIndexedParent(i);
        if(g.used != 0) {
          DataSerialization.write(out, i);
          DataSerialization.write(out, vec);
        }
      }
      DataSerialization.write(-1);
      // Binary format: (parent index,(lchild index, sparsevec of rchild,score)


    }
  }
}
*/

/**
 * Given a counter of productions that has been log-normalized by rows,
 * creates a grammar. Simple simple.
 */
