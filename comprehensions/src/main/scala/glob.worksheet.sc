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