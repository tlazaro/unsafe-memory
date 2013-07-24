package memory

import java.nio.{FloatBuffer, Buffer}
import memory.buffers.Buffers

sealed trait BufferBacked {
  this: Memory[_] =>

  val array: Buffer

  final def size(): Long = array.capacity

  override def toString(): String = {
    var res = "Vector[" + apply(0)
    var i = 1
    while (i < size) {
      res += ", " + apply(i)
      i += 1
    }
    res + "]"
  }
}

trait MemoryBankFloat extends Memory[Float] with BufferBacked {
  final def apply(i: Long): Float = array.asInstanceOf[FloatBuffer].get(i.toInt)

  final def update(i: Long, x: Float): Unit = array.asInstanceOf[FloatBuffer].put(i.toInt, x)
}

object FloatBufferMemoryBank extends MemoryBankFloat {
  val array = Buffers.createFloatBuffer(50)

  def clear() {
    array.clear()
  }
}

trait FloatBufferMemoryManager extends MemoryManager {
  override val data: Memory[Float] = FloatBufferMemoryBank
}
