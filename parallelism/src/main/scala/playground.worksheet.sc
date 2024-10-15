import parallelism.*

val numbers = List(2, 42, -3, 7, -8)
val names   = List("Mattia", "Hannah", "Leo", "Carla")

numbers.parMapThread(_ * 2)
names.parMapThread(_.length())

numbers.parMapFuture(_ * 2)
names.parMapFuture(_.length())


import BookStats.*
val book: Book = Vector(
    Vector("the", "quick", "brown", "fox", "is", "jumping", "over"), 
    Vector("the", "lazy", "dog")
)

length(book)

maxChapterLength(book)

countWord(book, "quick")
countWord(book, "the")

containsWord(book, "brown")
containsWord(book, "cat")

longestWord(book)

mostCommonWord(book)


import FoldReduce.* 
val nums = List(1, 2, 3, 4)

reduceWithFold(nums)(_ + _)
reduceWithFold(names)(_ ++ _)

reducePar(nums)(_ + _)
reducePar(names)(_ ++ _)