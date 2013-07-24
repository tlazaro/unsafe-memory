
import memory.{FloatUnsafe, MemoryManager}
import MemoryManager._
import model.{Vector3fArray, Vector3fPtr, Vector3f}

object Test extends Macros.TM with App {
  this.hello

  val a = Vector3f(0)

  a.x = 100
  a.y = 200
  a.z = 300

  val b = Vector3f(3)
  b.x = 5
  b.y = 15
  b.z = 25

  println(s"The buffer is: ${MemoryManager}")

  a :+ b
  a +: b

  println(s"The buffer is: ${MemoryManager}")

  val vs = new Vector3fArray(10, 5)

  vs(0) = a
  vs(1) = b

  val aptr = Vector3fPtr(a)
  val neoa = !aptr

  println(s"The buffer is: ${MemoryManager}")
}