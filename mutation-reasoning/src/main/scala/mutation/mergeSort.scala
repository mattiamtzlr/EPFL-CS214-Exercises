package mutation


def sort(a: Array[Int]): Array[Int] = {
  val n = a.length
  val b = new Array[Int](n)

  var width = 1

  while width < n do {
    var i = 0
    while i < n do {
      merge(a, b, i, width)
      i = i + (2 * width)
    }

    b.copyToArray(a)
    width = width * 2
  }

  a
}

def merge(a: Array[Int], b: Array[Int], indexLeft: Int, width: Int) =
  var i = indexLeft
  var j = indexLeft + width
  var z = indexLeft + (2 * width)

  for k <- indexLeft until z do {
    if i < indexLeft + width && (j >= z || a(i) <= a(j)) then {
      b(k) = a(i)
      i += 1
    } 
    else {
      b(k) = a(j)
      j += 1
    }
  }