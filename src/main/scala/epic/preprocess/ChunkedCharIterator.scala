package epic.preprocess

import java.text.CharacterIterator

/**
 * TODO
 *
 * @author dlwh
 **/
class ChunkedCharIterator(base: Iterator[Char]) extends CharacterIterator {

  private val buffer = new StringBuffer()
  private var pos = 0

  private def ensure(size: Int) = {
    while(buffer.length() < size && base.hasNext) {
      buffer.append(base.next())
    }
  }

  override def first(): Char = setIndex(0)

  override def next(): Char = {
    ensure(pos + 2)
    pos += 1
    if(pos >= buffer.length) {
     CharacterIterator.DONE
    } else {
      buffer.charAt(pos)
    }
  }

  override def setIndex(position: Int): Char = {
    ensure(position + 1)
    pos = position
    current()
  }

  override def getIndex: Int = pos

  override def last(): Char = {
    while(base.hasNext) {
      buffer.append(base.next())
    }

    buffer.charAt(buffer.length() - 1)

  }

  override def getBeginIndex: Int = 0

  override def getEndIndex: Int = {
    last()
    buffer.length()
  }

  override def current(): Char = {
    ensure(pos + 1)
    if(pos < 0 || pos >= buffer.length())
      CharacterIterator.DONE
    else
      buffer.charAt(pos)
  }

  override def previous(): Char = {
    pos -= 1
    current()
  }
}
