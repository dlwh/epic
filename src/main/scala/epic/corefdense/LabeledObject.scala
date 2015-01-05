package epic.corefdense

import edu.berkeley.nlp.futile.fig.basic.Indexer

trait LabeledObject[T] {
  def getLabel: T;
  
  def getIndexedLabel(indexer: Indexer[T]): Int = indexer.getIndex(getLabel)
}