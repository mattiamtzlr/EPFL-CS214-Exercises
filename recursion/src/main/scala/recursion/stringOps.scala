package recursion

def stringLength(s: String): Int =
  if s.isEmpty then 0
  else 1 + stringLength(s.tail)

def capitalizeString(s: String): String =
  if s.isEmpty then ""
  else s.head.toUpper.toString + capitalizeString(s.tail)

def discardWord(s: String): String =
  if s.isEmpty() then ""
  else
    if !s.head.isWhitespace then discardWord(s.tail)
    else s.head.toString + s.tail

def wordCount(s: String): Int =
  if s.isEmpty() then 0
  else 
    if s.head.isWhitespace then wordCount(s.tail)
    else 1 + wordCount(discardWord(s.tail))

def isBlank(s: String): Boolean =
  if s.isEmpty then true
  else s.head.isWhitespace && isBlank(s.tail)

def caesarCipher(s: String, shift: Int): String =
  val aCode = 'a'.toInt
  val zCode = 'z'.toInt
  if s.isEmpty then ""
  else 
    val code = s.head.toInt
    if code >= aCode && code <= zCode then
      ((code + shift - aCode) % 26 + aCode).toChar.toString + caesarCipher(s.tail, shift)
    else
      throw IllegalArgumentException("Only lowercase English letters are allowed!")

def reverseString(s: String): String =
  if s.isEmpty then ""
  else reverseString(s.tail) + s.head