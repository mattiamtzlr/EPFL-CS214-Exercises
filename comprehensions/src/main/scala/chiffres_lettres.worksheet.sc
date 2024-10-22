import comprehensions.*
val langs = List("Java", "Scala", "Rust", "C++", "Coq", "Lean")

onlyThreeLetterWords(langs)
louder(langs)
echo(langs, 3)
allTogether(langs, 3)
crossProduct(
    1 :: 2 :: 3 :: Nil, 'a' :: 'b' :: 'c' :: 'd' :: 'e' :: Nil
)
triangles((1, 2) :: (2, 3) :: (3, 1) :: Nil)


import DesLettres.*
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


import DesChiffres.*
val chars = 'a' :: 'b' :: 'c' :: 'd' :: 'e' :: Nil
val singleton = 'a' :: Nil
val duo = 'a' :: 'b' :: Nil

partitions(chars)
partitions(singleton)
partitions(duo)

// -5 * 3 + 75 / 6 == 10
val nums = List(3, 75, 6, -5)
val target = 10
allTrees(nums)
leCompteEstBon(nums, target)