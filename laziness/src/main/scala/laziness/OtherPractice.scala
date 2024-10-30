package laziness

import MyLazyList.*

object OtherPractice:

  lazy val codes: MyLazyList[String] =
    cons("0", cons("1", codes.flatMap(s => cons((s + "0"), cons((s + "1"), empty)))))

  lazy val palCodes: MyLazyList[String] =
    codes.filter(c => c.reverse == c)

  val middle: MyLazyList[String] = cons("", cons("0", cons("1", empty)))

  lazy val palCodes2: MyLazyList[String] =
    // need to add base cases at the beginning
    cons("0", cons("1", empty))
      .append(codes.flatMap(c => middle.map(m => c + m + c.reverse)))

  def nextLine(currentLine: List[Int]): List[Int] =
    currentLine.foldLeft[List[Int]](List.empty)((acc, x) =>
      acc match
        case y :: count :: rest if x == y => x :: (count + 1) :: rest
        case _                            => x :: 1 :: acc
    ).reverse

  lazy val funSeq: MyLazyList[List[Int]] =
    cons(List(1), funSeq.map(nextLine))
