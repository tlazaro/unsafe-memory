package memory

import memory.unsafe.UnsafeFloatArray

trait UnsafeFloatMemoryManager extends MemoryManager {
  override val data: Memory[Float] = FloatUnsafe
}


object FloatUnsafe extends UnsafeFloatArray(50) {

}