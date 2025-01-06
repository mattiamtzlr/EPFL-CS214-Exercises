import monads.*
import scala.util.*

for x <- Success("Hello, ")
    y <- Success("World!")
yield x + y // Success(Hello, World!)

for x <- Success("Hello, ")
    y <- Failure(Exception("Woops"))
yield x + y // Failure(Exception: Woops)

Try(throw Exception("Woops")) // Failure(Exception: Woops)

Success(1).flatMap(x => throw Exception("Woops")) // Failure(Exception: Woops)
