class Point(val x: Int, val y: Int) {
  def print(): Unit =
    println(s"x = $x, y = $y")

  def +(that: Point): Point =
    new Point(this.x + that.x, this.y + that.y)
}
