package memory.unsafe

import memory.Memory

object UnsafeArray {
  val BYTE = 1
  val CHAR = 2
  val SHORT = 2
  val INT = 4
  val FLOAT = 4
  val LONG = 8
  val DOUBLE = 8
}

class UnsafeByteArray(val size: Long) extends Memory[Byte] {

  import UnsafeArray._

  val address = ScalaUnsafe.getUnsafe.allocateMemory(size * BYTE)

  def update(i: Long, value: Byte): Unit = ScalaUnsafe.getUnsafe.putByte(address + i * BYTE, value)

  def apply(idx: Long) = ScalaUnsafe.getUnsafe.getByte(address + idx * BYTE)

  def clear() {
    var i = 0
    while (i < size) {
      update(i, 0)
      i += 1
    }
  }
}

class UnsafeFloatArray(val size: Long, clean: Boolean = true) extends Memory[Float] {

  import UnsafeArray._

  val address = ScalaUnsafe.getUnsafe.allocateMemory(size * FLOAT)
  if (clean) clear()

  def update(i: Long, value: Float): Unit = ScalaUnsafe.getUnsafe.putFloat(address + i * FLOAT, value)

  def apply(idx: Long) = ScalaUnsafe.getUnsafe.getFloat(address + idx * FLOAT)

  def clear() {
    var i = 0
    while (i < size) {
      update(i, 0)
      i += 1
    }
  }
}

class UnsafeIntArray(val size: Long) extends Memory[Int] {

  import UnsafeArray._

  val address = ScalaUnsafe.getUnsafe.allocateMemory(size * INT)

  def update(i: Long, value: Int): Unit = ScalaUnsafe.getUnsafe.putInt(address + i * INT, value)

  def apply(idx: Long) = ScalaUnsafe.getUnsafe.getInt(address + idx * INT)

  def clear() {
    var i = 0
    while (i < size) {
      update(i, 0)
      i += 1
    }
  }
}

