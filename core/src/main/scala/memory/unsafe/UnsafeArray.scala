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
    ScalaUnsafe.getUnsafe.setMemory(address, size * BYTE, 0)
  }

  def free() {
    ScalaUnsafe.getUnsafe.freeMemory(address)
  }
}

class UnsafeFloatArray(val size: Long, clean: Boolean = true) extends Memory[Float] {

  import UnsafeArray._

  val address = ScalaUnsafe.getUnsafe.allocateMemory(size * FLOAT)
  if (clean) clear()

  def update(i: Long, value: Float): Unit = ScalaUnsafe.getUnsafe.putFloat(address + i * FLOAT, value)

  def apply(idx: Long) = ScalaUnsafe.getUnsafe.getFloat(address + idx * FLOAT)

  def clear() {
    ScalaUnsafe.getUnsafe.setMemory(address, size * FLOAT, 0)
  }

  def free() {
    ScalaUnsafe.getUnsafe.freeMemory(address)
  }
}

class UnsafeIntArray(val size: Long) extends Memory[Int] {

  import UnsafeArray._
  import ScalaUnsafe._

  val address = getUnsafe.allocateMemory(size * INT)

  def update(i: Long, value: Int): Unit = getUnsafe.putInt(address + i * INT, value)

  def apply(idx: Long) = getUnsafe.getInt(address + idx * INT)

  def clear() {
    getUnsafe.setMemory(address, size * INT, 0)
  }

  def free() {
    getUnsafe.freeMemory(address)
  }
}

