package memory

trait Memory[T] {
  def update(i: Long, value: T): Unit

  def apply(i: Long): T

  def size: Long

  def clear()

  override def toString(): String = {
    var res = "Memory[" + apply(0)
    var i = 1
    while (i < size) {
      res += ", " + apply(i)
      i += 1
    }
    res + "]"
  }
}
