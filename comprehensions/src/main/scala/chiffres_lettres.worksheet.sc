import comprehensions.*
import DesLettres.*, DesChiffres.*

val langs = List("Java", "Scala", "Rust", "C++", "Coq", "Lean")

onlyThreeLetterWords(langs)
louder(langs)
echo(langs, 3)
allTogether(langs, 3)
crossProduct(
    1 :: 2 :: 3 :: Nil, 'a' :: 'b' :: 'c' :: 'd' :: 'e' :: Nil
)
triangles((1, 2) :: (2, 3) :: (3, 1) :: Nil)


val word = "POLYTECHNIQUE"
val scrambled = scramble(word)

val words = Set("ECOLE", "POLYTECHNIQUE", "FEDERALE", "DE", "LAUSANNE")
scrambleList(words)

val anagrams = Set("nale", "lean", "wasp", "swap", "sarg", "gras")
scrambleList(anagrams)

val exact = Set("nale", "lean", "lanes")
exactWord(exact, "aeln")

compatible(scrambled.distinct, scrambled)

longestWord(exact, "aelns")