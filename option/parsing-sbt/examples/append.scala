def append(x: List[Int], y: List[Int]): List[Int] = {
  if (x.isEmpty)
    y
  else {
    val h = x.head
    val t = x.tail
    h::append(t, y)
  }
}