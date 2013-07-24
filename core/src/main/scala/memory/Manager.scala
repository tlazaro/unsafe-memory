package memory

trait MemoryManager {
  val data: Memory[Float]

  val offset = 0

  def apply(i: Long): Float = data(i + offset)

  def update(i: Long, x: Float): Unit = data(i + offset) = x

  override def toString(): String = data.toString()
}

object MemoryManager {
  // Replace with different implementation FloatBufferMemoryManager or UnsafeFloatMemoryManager
  implicit val memory = new MemoryManager with UnsafeFloatMemoryManager

  override def toString(): String = memory.toString
}
