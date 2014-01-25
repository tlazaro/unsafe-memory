package memory.unsafe

import sun.misc.Unsafe

object ScalaUnsafe {
  val getUnsafe = {
    val f = classOf[Unsafe].getDeclaredField("theUnsafe")
    f.setAccessible(true)
    f.get(null).asInstanceOf[Unsafe]
  }
}

