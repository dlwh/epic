package scalanlp.parser;
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


import scalanlp.trees._;
import scalanlp.classify.Classifier;
import scalanlp.counters.Counters.DoubleCounter
import scalanlp.data._;

trait Parser[L,W] extends Classifier[Tree[L],Seq[W]] { outer =>
  def bestParse(s: Seq[W]) = classify(s);
  def map[U](f: Tree[L]=>Tree[U]):Parser[U,W] = new Parser[U,W] {
    override def scores(o: Seq[W]) = {
      val ctr = DoubleCounter[Tree[U]]();
      for( (t,w) <- outer.scores(o)) {
        val y = f(t);
        ctr(y) = ctr(y) max w;
      }
      ctr
    }
  }
}
