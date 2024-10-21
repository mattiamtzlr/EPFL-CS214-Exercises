package debugging

import java.nio.charset.Charset
import scala.jdk.CollectionConverters.*

/**
  * Returns all pairs of Charsets `(enc, dec)` such that 
  * ```
  * dec.decode(enc.encode(src)) == dst
  * ```
  * 
  * This is useful to figure out decoding errors.
  *
  * @param src the source string, i.e. the string which should be decoded
  * @param dst the string resulting from the incorrect decoding
  * @return Iterable of all pairs of Charsets as described above
  */
def findPairs(src: String, dst: String): Iterable[(Charset, Charset)] =
  val charsets = Charset.availableCharsets().values.asScala
  for 
    x <- charsets
    y <- charsets
    if x.canEncode() && y.decode(x.encode(src)).toString() == dst
  yield (x, y)
