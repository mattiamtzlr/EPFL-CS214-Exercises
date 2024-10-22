import comprehensions.*
import Glob.*

val p1 = "users.json"
val p2 = "20??-??-??.jpg"
val p3 = "icon.*"
val p4 = "20??-*.jp*g"
val p5 = "*"

glob(p1, "users.json")
glob(p1, "users.csv")

glob(p2, "2024-10-09.jpg")
glob(p2, "2024-10-09.jpeg")

glob(p3, "icon.png")
glob(p3, "icon.jpg")

glob(p4, "2024-cat.jpeg")
glob(p4, "2002-birthday.jpg")

glob(p5, "epfl")
glob(p5, "insane")


factorial(12)
factorial(19)
factorial(42)

fastExp(2, 3)
fastExp(5, 10)

decimalToBaseN(11, 2).mkString
decimalToBaseN(7482, 2).mkString
decimalToBaseN(57, 16).mkString

val unordered = 4 :: 2 :: -3 :: 9 :: 7 :: 1 :: -5 :: Nil
split(unordered)

val ordered1 = -3 :: 2 :: 4 :: Nil
val ordered2 = -5 :: 1 :: 7 :: 9 :: Nil
merge(ordered1, ordered2)

mergeSort(unordered)


filter_tracedIfTrue(7 to 32)
filter_traced(7 to 32)