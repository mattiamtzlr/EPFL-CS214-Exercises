import parallelism.*

val numbers = List(2, 42, -3, 7, -8)
val names   = List("Mattia", "Hannah", "Leo", "Carla")

numbers.parMapThread(_ * 2)
names.parMapThread(_.length())

numbers.parMapFuture(_ * 2)
names.parMapFuture(_.length())