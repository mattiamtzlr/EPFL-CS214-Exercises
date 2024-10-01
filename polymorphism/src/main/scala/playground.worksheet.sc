import poly.*
import MyList.*

2 + 2

val nums   = Cons(2, Cons(5, Cons(-3, Cons(8, Nil))))
val bools  = Cons(true, Cons(false, Cons(false, Cons(true, Nil))))
val fruits = Cons("apples", Cons("bananas", Cons("pears", Cons("strawberries", Nil))))

foldRight[Int, Int](nums)((_ + _), 0)
foldRight[Int, Int](nums)((_ * _), 1)
foldRight[Int, Boolean](nums)((h, t) => h != 0 && t, true)

foldRight[Boolean, Boolean](bools)((_ && _), true)
foldRight[Boolean, Boolean](bools)((_ || _), false)

foldRight[String, String](fruits)((_ ++ _), "")
foldRight[String, Int](fruits)((h, t) => h.length() + t, 0)