package hoare

def abs(x: Int): Int = {
  if x < 0 then -x else x
} ensuring (res =>
  res >= 0
)



def find(l: List[Int], p: Int => Boolean): Option[Int] = {
  var li = l
  while !li.isEmpty do

    assert(l.take(l.size - li.size).forall(!p(_)))

    if p(li.head) then
      return Some(li.head)
    li = li.tail

    assert(l.take(l.size - li.size).forall(!p(_)))

  None
} ensuring(res => 
  res.isEmpty || (p(res.get) && l.contains(res.get) && l.take(l.indexOf(res.get)).forall(!p(_)))
)



def maxLoopList(l: List[Int]): Int = {
  require(!l.isEmpty)
  var list = l.tail
  var max  = l.head

  while !list.isEmpty do {
    if list.head > max then max = list.head
    list = list.tail
  }

  max
}

def maxLoopArray(a: Array[Int]): Int = {
  require(!a.isEmpty)
  var i   = 1
  var max = a(0)

  while i < a.length do {
    if a(i) > max then max = a(i)
    i += 1
  }

  max
}

def maxLoopListWithInvariant(l: List[Int]): Int =
  require(!l.isEmpty)

  def invariant(numProc: Int, currMax: Int): Boolean = 
    l.take(numProc).forall(_ <= currMax) && l.take(numProc).contains(currMax)

  var list = l.tail
  var max  = l.head
  assert(invariant(l.size - list.size, max))

  while !list.isEmpty do {
    if list.head > max then max = list.head
    list = list.tail
    assert(invariant(l.size - list.size, max))
  }

  max

def maxLoopArrayWithInvariant(a: Array[Int]): Int =
  require(a.size > 0)

  def invariant(numProc: Int, currMax: Int): Boolean = 
    a.take(numProc).forall(_ <= currMax) && a.take(numProc).contains(currMax)

  var i   = 1
  var max = a(0)
  assert(invariant(i, max))

  while i < a.length do {
    if a(i) > max then max = a(i)
    i += 1
    assert(invariant(i, max))
  }

  max