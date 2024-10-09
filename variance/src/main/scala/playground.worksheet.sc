import variance.*

val fruits = "apples" :: "bananas" :: "cherries" :: "grapes" :: "lemons" :: "oranges" :: "strawberries" :: Nil
val shapes = "circles" :: "disks" :: "ovals" :: "polygons" :: "squares" :: "triangles" :: Nil
val names  = "arthur" :: "annika" :: "carla" :: "hannah" :: "leo" :: "leo" :: "olivia" :: "mattia" :: Nil

val fruitStack = ListStack(fruits)
val shapeStack = ListStack(shapes)
val nameStack  = ListStack(names)

fruitStack.peek()
val (popped1, rest1) = fruitStack.pop()
val (popped2, rest2) = rest1.pop()

val pushed = fruitStack.push("watermelons")
val (pushPopped, pushRest) = pushed.pop()
pushRest == fruitStack

val joined = joinStacks(
    fruitStack :: shapeStack :: nameStack :: Nil
)

