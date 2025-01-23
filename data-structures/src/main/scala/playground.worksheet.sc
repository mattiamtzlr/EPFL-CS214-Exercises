import ds.*

val data  = List(42, 37, 11)
val stack = StackImpl[Int](data)

val (first,  rest1) = stack.pop.get
val (second, rest2) = rest1.pop.get
val (third,  rest3) = rest2.pop.get

rest3.isEmpty


val names0 = Stack[String]()
names0.isEmpty

val names1 = names0.push("mattia")
val names2 = names1.push("hannah")
val names3 = names2.push("leo")
val names4 = names3.push("carla")

names4.pop.get._2.pop.get._2 == names2



val deque0 = Deque[Int]()
val deque1 = deque0.pushFirst(2)
val deque2 = deque1.pushFirst(1)
val deque3 = deque2.pushLast (3)
val deque4 = deque3.pushLast (4)

deque4.popFirstAll
deque4.popLastAll



val trie0 = TrieImpl[Int]()
val trie1 = trie0.insert("apples", 4).insert("kiwis", 17).insert("mangos", 5)
trie1.find("kiwis")


val dict = tokenize("I like apples and kiwis")
val spellchecker = TrieSpellChecker(dict)

spellchecker.checkText("I lyke ampples and kivis")
