import poly.*
import MyList.*

2 + 2

val nums   = Cons(13, Cons(5, Cons(-3, Cons(8, Nil))))
val bools  = Cons(true, Cons(false, Cons(false, Nil)))
val fruits = Cons("apples", Cons("bananas", Cons("pears", Cons("strawberries", Nil))))

foldRight[Int, Int](nums)((_ + _), 0)
foldRight[Int, Int](nums)((_ * _), 1)
foldRight[Int, Boolean](nums)((h, t) => h != 0 && t, true)

foldRight[Boolean, Boolean](bools)((_ && _), true)
foldRight[Boolean, Boolean](bools)((_ || _), false)

foldRight[String, String](fruits)((_ ++ _), "")
foldRight[String, Int](fruits)((h, t) => h.length() + t, 0)

forall(nums)(_ > -5)
exists(nums)(_ == -3)

zip(nums, bools)
zipWith(nums, fruits)((i, s) => i > s.length())

takeWhilePositive(nums)

val chars = Cons('I', Cons(' ', Cons('a', Cons('m', Cons(' ', Cons('s', Cons('t', Cons('e', Cons('v', Cons('e', Nil))))))))))
wordCount(chars)

flatMap(nums)(n => Cons(n, Cons(n * 2, Nil)))

val mains = Cons("Burger", Cons("Pizza", Cons("Pasta", Nil)))
val sides = Cons("Salad", Cons("Soup", Nil))

crossProduct(mains, sides)

val edges = Cons((1, 2), Cons((2, 3), Cons((3, 1), Nil)))
triangles(edges)

foldLeft(fruits)("", (_ ++ _))
sum0Fold(nums)
sum1Fold(nums)
countEven(nums)
totalLength(fruits)

curriedZipWith[Int, String, Boolean](nums, fruits)((i, s) => i > s.length())

compose[Int, Boolean, String](b => if b then "greater " else "less or equal")(i => i > 0)(5)