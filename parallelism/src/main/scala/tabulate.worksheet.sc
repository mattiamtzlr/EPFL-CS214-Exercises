import parallelism.* 

import scala.collection.parallel.mutable.ParArray

Array.seqTabulate(8)(i => math.pow(2, i))
    .mkString("[", ", ", "]")
Array.seqTabulate(26)(i => (i + 65).toChar)
    .mkString

ParArray.parTabulate(8)(i => math.pow(2, i))
    .mkString("[", ", ", "]")
ParArray.parTabulate(26)(i => (i + 65).toChar)
    .mkString

vectorAdd(Array(1, 2, 3), Array(4, 5, 6))