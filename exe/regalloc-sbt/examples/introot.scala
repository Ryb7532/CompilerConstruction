def increase(k: Int, n:Int): Int =
  if (n < (k + 1) * (k + 1)) k
  else k + 1

def introot(n: Int): Int =
  if (n == 0) 0
  else increase(2 * introot(n / 4), n)

def test(n: Int): Int = introot(n)
