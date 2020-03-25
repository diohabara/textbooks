trait Adder[T] {
  def zero: T
  def plus(x: T, y: T): T
}

object IntAdder extends Adder[Int] {
  def zero: Int = 0
  def plus(x: Int, y: Int): Int = x + y

}

