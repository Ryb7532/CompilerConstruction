def append(x: List[Int], y: List[Int]): List[Int] = {
  if (x.isEmpty)
    y
  else {
    val h = x.head
    val t = x.tail
    h::append(t, y)
  }
}

def test(): List[Int] = append(1::2::3::Nil, 4::5::Nil)