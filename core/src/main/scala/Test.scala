
import memory.MemoryManager
import memory.unsafe.ScalaUnsafe._
import memory.unsafe.UnsafeIntArray
import MemoryManager._
import model.Vector3fPtr
import model.{Vector3fArray, Vector3fPtr, Vector3f}
import scala.util.Random
import sun.misc.Unsafe

object Test extends Macros.TM with App {
  this.hello

  //  val a = Vector3f(0)
  //
  //  a.x = 100
  //  a.y = 200
  //  a.z = 300
  //
  //  val b = Vector3f(3)
  //  b.x = 5
  //  b.y = 15
  //  b.z = 25
  //
  //  println(s"The buffer is: ${MemoryManager}")
  //
  //  a :+ b
  //  a +: b
  //
  //  println(s"The buffer is: ${MemoryManager}")
  //
  //  val vs = new Vector3fArray(10, 5)
  //
  //  vs(0) = a
  //  vs(1) = b
  //
  //  val aptr = Vector3fPtr(a)
  //  val neoa = !aptr
  //
  //  println(s"The buffer is: ${MemoryManager}")

  case class Bench(title: String, size: Int, loaded: Long, folded: Long, result: Int)
  case class BenchLong(title: String, size: Int, loaded: Long, folded: Long, result: Long)
  case class BenchFloat(title: String, size: Int, loaded: Long, folded: Long, result: Float)
  case class BenchDouble(title: String, size: Int, loaded: Long, folded: Long, result: Double)

  def benchArray(seed: Int, size: Int): Bench = {
    val random = new Random(seed)
    val array = new Array[Int](size)

    val start = System.nanoTime()

    var i = 0
    while (i < array.length) {
      array(i) = random.nextInt()
      i += 1
    }

    val loaded = System.nanoTime()

    var j = 0
    var result = 0
    while (j < array.length) {
      result += array(j)
      j += 1
    }

    val folded = System.nanoTime()

    Bench("Array", size, loaded - start, folded - loaded, result)
  }

  val f = classOf[Unsafe].getDeclaredField("theUnsafe")
  f.setAccessible(true)
  val unsafe = f.get(null).asInstanceOf[Unsafe]

  def benchUnsafe(seed: Int, size: Int): Bench = {
    val address = unsafe.allocateMemory(size * 4)
    val random = new Random(seed)

    val start = System.nanoTime()

    val end = address + size * 4
    var i = address
    while (i < end) {
      unsafe.putInt(i, random.nextInt())
      i += 4
    }

    val loaded = System.nanoTime()

    var j = address
    var result = 0
    while (j < end) {
      result += unsafe.getInt(j)
      j += 4
    }

    val folded = System.nanoTime()

    unsafe.freeMemory(address)

    Bench("Unsafe array", size, loaded - start, folded - loaded, result)
  }

  val seed = 142857
  val values = List(3, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 20000000)
  for (value <- values) {
    val bench1 = benchArray(seed, value)
    val bench2 = benchUnsafe(seed, value)

    println(bench1)
    println(bench2)

    assert(bench1.result == bench2.result)

    println("Ratio 'loaded': " + (bench2.loaded.toFloat / bench1.loaded))
    println("Ratio 'folded': " + (bench2.folded.toFloat / bench1.folded))
    println()
  }

  def benchArrayLong(seed: Int, size: Int): BenchLong = {
    val random = new Random(seed)
    val array = new Array[Long](size)

    val start = System.nanoTime()

    var i = 0
    while (i < array.length) {
      array(i) = random.nextLong()
      i += 1
    }

    val loaded = System.nanoTime()

    var j = 0
    var result = 0L
    while (j < array.length) {
      result += array(j)
      j += 1
    }

    val folded = System.nanoTime()

    BenchLong("Array double", size, loaded - start, folded - loaded, result)
  }

  def benchUnsafeLong(seed: Int, size: Int): BenchLong = {
    val address = unsafe.allocateMemory(size * 8)
    val random = new Random(seed)

    val start = System.nanoTime()

    val end = address + size * 8
    var i = address
    while (i < end) {
      unsafe.putLong(i, random.nextLong())
      i += 8
    }

    val loaded = System.nanoTime()

    var j = address
    var result = 0L
    while (j < end) {
      result += unsafe.getLong(j)
      j += 8
    }

    val folded = System.nanoTime()

    unsafe.freeMemory(address)

    BenchLong("Unsafe longs", size, loaded - start, folded - loaded, result)
  }

  println("\nLong tests\n")
  for (value <- values) {
    val bench1 = benchArrayLong(seed, value)
    val bench2 = benchUnsafeLong(seed, value)

    println(bench1)
    println(bench2)

    assert(bench1.result == bench2.result)

    println("Ratio 'loaded': " + (bench2.loaded.toFloat / bench1.loaded))
    println("Ratio 'folded': " + (bench2.folded.toFloat / bench1.folded))
    println()
  }

  def benchArrayFloat(seed: Int, size: Int): BenchFloat = {
    val random = new Random(seed)
    val array = new Array[Float](size)

    val start = System.nanoTime()

    var i = 0
    while (i < array.length) {
      array(i) = random.nextFloat()
      i += 1
    }

    val loaded = System.nanoTime()

    var j = 0
    var result = 0f
    while (j < array.length) {
      result += array(j)
      j += 1
    }

    val folded = System.nanoTime()

    BenchFloat("Array float", size, loaded - start, folded - loaded, result)
  }

  def benchUnsafeFloat(seed: Int, size: Int): BenchFloat = {
    val address = unsafe.allocateMemory(size * 4)
    val random = new Random(seed)

    val start = System.nanoTime()

    val end = address + size * 4
    var i = address
    while (i < end) {
      unsafe.putFloat(i, random.nextFloat())
      i += 4
    }

    val loaded = System.nanoTime()

    var j = address
    var result = 0f
    while (j < end) {
      result += unsafe.getFloat(j)
      j += 4
    }

    val folded = System.nanoTime()

    unsafe.freeMemory(address)

    BenchFloat("Unsafe floats", size, loaded - start, folded - loaded, result)
  }

  println("\nFloat tests\n")
  for (value <- values) {
    val bench1 = benchArrayFloat(seed, value)
    val bench2 = benchUnsafeFloat(seed, value)

    println(bench1)
    println(bench2)

    assert(bench1.result == bench2.result)

    println("Ratio 'loaded': " + (bench2.loaded.toFloat / bench1.loaded))
    println("Ratio 'folded': " + (bench2.folded.toFloat / bench1.folded))
    println()
  }

  def benchArrayDouble(seed: Int, size: Int): BenchDouble = {
    val random = new Random(seed)
    val array = new Array[Double](size)

    val start = System.nanoTime()

    var i = 0
    while (i < array.length) {
      array(i) = random.nextDouble()
      i += 1
    }

    val loaded = System.nanoTime()

    var j = 0
    var result = 0.0
    while (j < array.length) {
      result += math.sqrt(array(j))
      j += 1
    }

    val folded = System.nanoTime()

    BenchDouble("Array double", size, loaded - start, folded - loaded, result)
  }

  def benchUnsafeDouble(seed: Int, size: Int): BenchDouble = {
    val address = unsafe.allocateMemory(size * 8)
    val random = new Random(seed)

    val start = System.nanoTime()

    val end = address + size * 8
    var i = address
    while (i < end) {
      unsafe.putDouble(i, random.nextDouble())
      i += 8
    }

    val loaded = System.nanoTime()

    var j = address
    var result = 0.0
    while (j < end) {
      result += math.sqrt(unsafe.getDouble(j))
      j += 8
    }

    val folded = System.nanoTime()

    unsafe.freeMemory(address)

    BenchDouble("Unsafe doubles", size, loaded - start, folded - loaded, result)
  }

  println("\nDouble tests\n")
  for (value <- values) {
    val bench1 = benchArrayDouble(seed, value)
    val bench2 = benchUnsafeDouble(seed, value)

    println(bench1)
    println(bench2)

    assert(bench1.result == bench2.result)

    println("Ratio 'loaded': " + (bench2.loaded.toFloat / bench1.loaded))
    println("Ratio 'folded': " + (bench2.folded.toFloat / bench1.folded))
    println()
  }
}