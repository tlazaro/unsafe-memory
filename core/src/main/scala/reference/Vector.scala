package reference

import java.nio.IntBuffer
import java.nio.DoubleBuffer
import java.nio.FloatBuffer
import java.nio.Buffer
import memory.buffers.Buffers

// TODO somehow test if this is working, check .class files and see what happened
sealed trait M3DVector[T] {
  @inline def array: Buffer

  @inline def apply(i: Int): T

  @inline def update(i: Int, x: T): Unit

  @inline final def size(): Int = array.capacity

  @inline def copy(other: Array[T]): Unit

  @inline def copy(other: M3DVector[T]): Unit

  @inline def copy(offset: Int, other: M3DVector[T], otherOffset: Int, length: Int): Unit

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

sealed trait M3DVectorFloat extends M3DVector[Float] {
  @inline final def apply(i: Int): Float = array.asInstanceOf[FloatBuffer].get(i)

  @inline final def update(i: Int, x: Float): Unit = array.asInstanceOf[FloatBuffer].put(i, x)

  @inline final def copy(other: Array[Float]): Unit = {
    array.position(0)
    array.asInstanceOf[FloatBuffer].put(other)
  }

  @inline final def copy(other: M3DVector[Float]): Unit = {
    array.position(0)
    other.array.position(0)
    array.asInstanceOf[FloatBuffer].put(other.array.asInstanceOf[FloatBuffer])
  }

  @inline final def copy(offset: Int, other: M3DVector[Float], otherOffset: Int, length: Int): Unit = {
    array.position(offset)
    other.array.position(otherOffset)
    other.array.limit(otherOffset + length)
    array.asInstanceOf[FloatBuffer].put(other.array.asInstanceOf[FloatBuffer])
    other.array.clear()
  }
}

sealed trait M3DVectorDouble extends M3DVector[Double] {
  @inline final def apply(i: Int): Double = array.asInstanceOf[DoubleBuffer].get(i)

  @inline final def update(i: Int, x: Double): Unit = array.asInstanceOf[DoubleBuffer].put(i, x)

  @inline final def copy(other: Array[Double]): Unit = {
    array.position(0)
    array.asInstanceOf[DoubleBuffer].put(other)
  }

  @inline final def copy(other: M3DVector[Double]): Unit = {
    array.position(0)
    other.array.position(0)
    array.asInstanceOf[FloatBuffer].put(other.array.asInstanceOf[FloatBuffer])
  }

  @inline final def copy(offset: Int, other: M3DVector[Double], otherOffset: Int, length: Int): Unit = {
    array.position(offset)
    other.array.position(otherOffset)
    other.array.limit(otherOffset + length)
    array.asInstanceOf[DoubleBuffer].put(other.array.asInstanceOf[DoubleBuffer])
    other.array.clear()
  }
}

sealed trait M3DVectorInt extends M3DVector[Int] {
  @inline final def apply(i: Int): Int = array.asInstanceOf[IntBuffer].get(i)

  @inline final def update(i: Int, x: Int): Unit = array.asInstanceOf[IntBuffer].put(i, x)

  @inline final def copy(other: Array[Int]): Unit = {
    array.position(0)
    array.asInstanceOf[IntBuffer].put(other)
  }

  @inline final def copy(other: M3DVector[Int]): Unit = {
    array.position(0)
    other.array.position(0)
    array.asInstanceOf[IntBuffer].put(other.array.asInstanceOf[IntBuffer])
  }

  @inline final def copy(offset: Int, other: M3DVector[Int], otherOffset: Int, length: Int): Unit = {
    array.position(offset)
    other.array.position(otherOffset)
    other.array.limit(otherOffset + length)
    array.asInstanceOf[IntBuffer].put(other.array.asInstanceOf[IntBuffer])
    other.array.clear()
  }
}

object M3DVector {
  def apply(x: Float, y: Float, z: Float, w: Float) = {
    val vec = new M3DVector4f
    vec(0) = x
    vec(1) = y
    vec(2) = z
    vec(3) = w
    vec
  }

  def apply(x: Float, y: Float, z: Float) = {
    val vec = new M3DVector3f
    vec(0) = x
    vec(1) = y
    vec(2) = z
    vec
  }

  // Write and Read to Vector don't depend on buffer position but reading the buffer directly needs position reset.
  // This is the best place to do so because it's the only place that will be needed
  implicit def M3DVectorIntToBuffer(vec: M3DVectorInt) = {
    vec.array.position(0); vec.array.asInstanceOf[IntBuffer]
  }

  implicit def M3DVectorFloatToBuffer(vec: M3DVectorFloat) = {
    vec.array.position(0); vec.array.asInstanceOf[FloatBuffer]
  }

  implicit def M3DVectorDoubleToBuffer(vec: M3DVectorDouble) = {
    vec.array.position(0); vec.array.asInstanceOf[DoubleBuffer]
  }
}

// 3D points = 3D Vectors, but we need a 2D representations sometimes... (v,y) order
final class M3DVector2f(private[this] val a: FloatBuffer = Buffers.createFloatBuffer(2)) extends M3DVectorFloat {
  @inline override def array = a
}

final class M3DVector2d(private[this] val a: DoubleBuffer = Buffers.createDoubleBuffer(2)) extends M3DVectorDouble {
  @inline override def array = a
}

// Vector of three floats (v, y, z) Vector of three doubles (v, y, z)
final class M3DVector3f(private[this] val a: FloatBuffer = Buffers.createFloatBuffer(3)) extends M3DVectorFloat {
  @inline override def array = a
}

final class M3DVector3d(private[this] val a: DoubleBuffer = Buffers.createDoubleBuffer(3)) extends M3DVectorDouble {
  @inline override def array = a
}

// Lesser used... Do we really need these? Yes, occasionaly we do need a trailing w component
final class M3DVector4f(private[this] val a: FloatBuffer = Buffers.createFloatBuffer(4)) extends M3DVectorFloat {
  @inline override def array = a
}

final class M3DVector4d(private[this] val a: DoubleBuffer = Buffers.createDoubleBuffer(4)) extends M3DVectorDouble {
  @inline override def array = a
}

final class M3DVector4i(private[this] val a: IntBuffer = Buffers.createIntBuffer(4)) extends M3DVectorInt {
  @inline override def array = a
}

trait M3DVectorArray[T <: M3DVector[_]]

// Creates an array of Vectors that share a common FloatBuffer
final class M3DVector4fArray(private val buffer: FloatBuffer) extends M3DVectorArray[M3DVector4f] {
  def this(size: Int) = this(Buffers.createFloatBuffer(4 * size))

  private[this] val vectors = new Array[M3DVector4f](buffer.capacity / 4)

  for (i <- 0 until vectors.length) {
    buffer.position(i * 4)
    buffer.limit(i * 4 + 4)
    vectors(i) = new M3DVector4f(buffer.slice)
  }
  buffer.clear()

  @inline final def apply(i: Int): M3DVector4f = vectors(i)

  def slice(length: Int, start: Int = 0) = {
    buffer.position(start * 4)
    buffer.limit(4 * (start + length))

    val res = buffer.slice

    buffer.clear()

    res
  }
}

// Creates an array of Vectors that share a common FloatBuffer
final class M3DVector3fArray(private val buffer: FloatBuffer) extends M3DVectorArray[M3DVector3f] {
  def this(size: Int) = this(Buffers.createFloatBuffer(3 * size))

  private[this] val vectors = new Array[M3DVector3f](buffer.capacity / 3)

  for (i <- 0 until vectors.length) {
    buffer.position(i * 3)
    buffer.limit(i * 3 + 3)
    vectors(i) = new M3DVector3f(buffer.slice)
  }
  buffer.clear()

  @inline final def apply(i: Int): M3DVector3f = vectors(i)

  def slice(length: Int, start: Int = 0) = {
    buffer.position(start * 3)
    buffer.limit(3 * (start + length))

    val res = buffer.slice

    buffer.clear()

    res
  }
}

// Creates an array of Vectors that share a common FloatBuffer
final class M3DVector2fArray(private val buffer: FloatBuffer) extends M3DVectorArray[M3DVector2f] {
  def this(size: Int) = this(Buffers.createFloatBuffer(2 * size))

  private[this] val vectors = new Array[M3DVector2f](buffer.capacity / 2)

  for (i <- 0 until vectors.length) {
    buffer.position(i * 2)
    buffer.limit(i * 2 + 2)
    vectors(i) = new M3DVector2f(buffer.slice)
  }
  buffer.clear()

  @inline final def apply(i: Int): M3DVector2f = vectors(i)

  def slice(length: Int, start: Int = 0) = {
    buffer.position(start * 2)
    buffer.limit(2 * (start + length))

    val res = buffer.slice

    buffer.clear()

    res
  }
}

// Creates an array of Vectors that share a common FloatBuffer
final class M3DMatrix44fArray(private val buffer: FloatBuffer) extends M3DVectorArray[M3DMatrix44f] {
  def this(size: Int) = this(Buffers.createFloatBuffer(2 * size))

  private[this] val matrixes = new Array[M3DMatrix44f](buffer.capacity / 16)

  for (i <- 0 until matrixes.length) {
    buffer.position(i * 16)
    buffer.limit(i * 16 + 16)
    matrixes(i) = new M3DMatrix44f(buffer.slice)
  }
  buffer.clear()

  @inline final def apply(i: Int): M3DMatrix44f = matrixes(i)

  def slice(length: Int, start: Int = 0) = {
    buffer.position(start * 16)
    buffer.limit(16 * (start + length))

    val res = buffer.slice

    buffer.clear()

    res
  }
}

object M3DVector4fArray {
  def apply(nums: Float*): M3DVector4fArray = {
    val ret = new M3DVector4fArray(nums.length)
    ret.buffer.position(0)
    ret.buffer.put(nums.toArray[Float])
    ret.buffer.flip()
    ret
  }

  implicit def toFloatBuffer(vecArray: M3DVector4fArray) = vecArray.buffer
}

object M3DVector3fArray {
  def apply(nums: Float*): M3DVector3fArray = {
    val ret = new M3DVector3fArray(nums.length)
    ret.buffer.position(0)
    ret.buffer.put(nums.toArray[Float])
    ret.buffer.flip()
    ret
  }

  implicit def toFloatBuffer(vecArray: M3DVector3fArray) = vecArray.buffer
}

object M3DVector2fArray {
  def apply(nums: Float*): M3DVector2fArray = {
    val ret = new M3DVector2fArray(nums.length)
    ret.buffer.position(0)
    ret.buffer.put(nums.toArray[Float])
    ret.buffer.flip()
    ret
  }

  implicit def toFloatBuffer(vecArray: M3DVector2fArray) = vecArray.buffer
}

// 3x3 matrix - column major. X vector is 0, 1, 2, etc.
// 0 3 6
// 1 4 7
// 2 5 8

// A 3 v 3 matrix, column major (floats) - OpenGL Style
final class M3DMatrix33f(private[this] val a: FloatBuffer = Buffers.createFloatBuffer(9)) extends M3DVectorFloat {
  @inline override def array = a
}

// A 3 v 3 matrix, column major (doubles) - OpenGL Style
final class M3DMatrix33d(private[this] val a: DoubleBuffer = Buffers.createDoubleBuffer(9)) extends M3DVectorDouble {
  @inline override def array = a
}

// 4x4 matrix - column major. X vector is 0, 1, 2, etc.
// 0 4 8 12
// 1 5 9 13
// 2 6 10 14
// 3 7 11 15
//
// A 4 v 4 matrix, column major (floats) - OpenGL Style
final class M3DMatrix44f(private[this] val a: FloatBuffer = Buffers.createFloatBuffer(16)) extends M3DVectorFloat {
  @inline override def array = a
}

// A 4 v 4 matrix, column major (doubles) - OpenGL Style
final class M3DMatrix44d(private[this] val a: DoubleBuffer = Buffers.createDoubleBuffer(16)) extends M3DVectorDouble {
  @inline override def array = a
}