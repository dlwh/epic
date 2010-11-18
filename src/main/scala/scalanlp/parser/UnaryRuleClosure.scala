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

import scalanlp.math.Semiring;
import scalala.tensor.sparse._;
import scalanlp.graphs._;

/**
 *
 * @author dlwh
 */
@serializable
@SerialVersionUID(1)
trait UnaryRuleClosure {
  def closeFromChild(child: Int):SparseVector;
  def closeFromParent(parent: Int):SparseVector;
}

/**
 * 
 * @author dlwh
 */
object UnaryRuleClosure {

  class UnaryClosureException(cause: Exception) extends Exception("Error computing closure",cause);

  def apply[L](g: Grammar[L])(implicit semiring: Semiring[Double]):UnaryRuleClosure = {
    computeClosure(g)(semiring);
  }

  def computeClosure[L](g:Grammar[L])(implicit semiring: Semiring[Double]):UnaryRuleClosure = try {
    val unaryGraph = computeUnaryGraph(g);
    val childToParent = Distance.allPairDistances(unaryGraph);
    val parentToChild = Distance.allPairDistances(scalanlp.graphs.reverseWeighted(unaryGraph));
    val indexedC2P = indexMap(g, childToParent);
    val indexedP2C = indexMap(g, parentToChild)
    new UnaryRuleClosure {
      def closeFromChild(c: Int) = indexedC2P(c);
      def closeFromParent(p: Int) = indexedP2C(p);
    }
  } catch {
    case ex: ArithmeticException =>
      throw new UnaryClosureException(ex);

  }

  private def indexMap[L](grammar: Grammar[L], map: Map[Int,Map[Int,Double]])(implicit semiring: Semiring[Double]):Array[SparseVector] = {
    Array.tabulate(grammar.index.size){i =>
      val sparse = grammar.mkSparseVector(Double.NegativeInfinity);
      for( (k,v) <- map.getOrElse(i,Map.empty) if v != semiring.zero) sparse(k) = v;
      sparse
    };
  }

  // graph points child to parent.
  def computeUnaryGraph[L](g: Grammar[L]): WeightedDigraph[Int,(Int,Int,Double),Double] = {
    new Digraph[Int,(Int,Int,Double)] with Weighted[Int,(Int,Int,Double),Double] {

      type Edge = (Int,Int,Double);
      def source(e: Edge) = e._1;
      def sink(e: Edge) = e._2;
      def weight(e: Edge) = e._3;

      def getEdge(n1: Int, n2: Int) = {
        val w = g.unaryRulesByIndexedChild(n1)(n2)
        if(w == Double.NegativeInfinity) None
        else Some((n1,n2,w));
      }

      def nodes = 0 until g.index.size;
      def edges = for(n1 <- nodes iterator; e <- edgesFrom(n1)) yield e;

      def successors(n1: Int) = g.unaryRulesByIndexedChild(n1).keysIterator;
      def edgesFrom(n1: Int) = {
        for( (n2,w) <- g.unaryRulesByIndexedChild(n1).activeElements)
          yield (n1,n2,w);
      }

      override def toString = {
        edges.map{ case (n1,n2,w) => (g.index.get(n1), g.index.get(n2), w)}.mkString("Unaries[",",\n","]");
      }

    }
  }

}