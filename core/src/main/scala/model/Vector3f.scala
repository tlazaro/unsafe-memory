package model

import memory.MemoryManager

case class Vector3f(val ptr: Int) extends AnyVal {
  def x(implicit m: MemoryManager): Float = m(ptr + 0)

  def y(implicit m: MemoryManager): Float = m(ptr + 1)

  def z(implicit m: MemoryManager): Float = m(ptr + 2)

  def x_=(value: Float)(implicit m: MemoryManager) = m(ptr + 0) = value

  def y_=(value: Float)(implicit m: MemoryManager) = m(ptr + 1) = value

  def z_=(value: Float)(implicit m: MemoryManager) = m(ptr + 2) = value

  def :+(other: Vector3f)(implicit m: MemoryManager) = {
    x_=(x + other.x)
    y_=(y + other.y)
    z_=(z + other.z)
  }

  def +:(other: Vector3f)(implicit m: MemoryManager) = this :+ other

  /** May come in handy for some type checking. Forcing the user to signal when passing as a pointer. */
  def unary_~ = Vector3fPtr(ptr)
}

object Vector3f {
  implicit def vector3fToInt(v: Vector3f) = v.ptr
}

/** May come in handy for some type checking. Forcing the user to signal when passing as a pointer. */
case class Vector3fPtr(val ptr: Int) extends AnyVal {
  def unary_! = Vector3f(ptr)
}

final class Vector3fArray(val ptr: Int, var length: Int) {
  def apply(i: Int)(implicit m: MemoryManager) = Vector3f(ptr + i * 3)

  def update(i: Int, x: Float, y: Float, z: Float)(implicit m: MemoryManager): Unit = {
    m(ptr + i * 3) = x
    m(ptr + i * 3 + 1) = y
    m(ptr + i * 3 + 2) = z
  }

  def update(i: Int, v: Vector3f)(implicit m: MemoryManager): Unit = {
    update(i, v.x, v.y, v.z)
  }
}