package strongestPostcondition
// Verified with Stainless, for validity.
// The conditions are then correct but might be not strong enough.

def f(x: BigInt): BigInt = {
  require(x > 0)
  x + 1
} ensuring (y => y > 1)

def f1(x: BigInt): BigInt = {
  require(x > 0)
  2 * x
} ensuring (y => y > 1 && y % 2 == 0)

def f2(x: BigInt): BigInt = {
  require(x > 2 && x <= 10)
  if x < 5 then BigInt(0) else x
} ensuring (y => y == 0 || (y >= 5 && y <= 10))

def f3(x: BigInt): BigInt = {
  require((x > 0 && x < 9) || (x >= 20 && x < 26))
  if x < 6 then x + 1
  else if x < 23 then 3 * x
  else -2 * x
} ensuring (y => 
  (y >= 2 && y <= 6)
  || (((y >= 60 && y <= 66) || (y >= -16 && y <= -12) || (y >= -52 && y <= -46)) && y % 2 == 0)
)

def imperativeF1(x: BigInt): BigInt = {
  require(x >= -1 && x <= 4)
  var y = x
  var z = BigInt(1)
  if y > 0 then z *= 4
  if y < 4 then z *= 2
  if y % 2 == 0 then z -= 3

  z
} ensuring (z => ???)

def imperativeF2(x: BigInt): BigInt = {
  require(x >= -5 && x <= 5)
  var y = x
  var z = BigInt(0)
  while y * y > 0 do
    z += 1
    if y > 0 then y -= 1
    else y += 1
  z
} ensuring (z => ???)
