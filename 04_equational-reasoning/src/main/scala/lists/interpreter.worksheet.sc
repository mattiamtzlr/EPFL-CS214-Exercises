import lists.* 
import Instruction.*
// A B C - * D E + + === A * (B - C) + (D + E)
val A = 2; val B = 3; val C = 1; val D = 5; val E = 4 // => final stack value: 13
val program = (
    Push(A) :: Push(B) :: Push(C) :: Minus :: Mul :: 
    Push(D) :: Push(E) :: Add :: Add :: Nil
)

val stack5: List[Int] = 4 :: -9 :: 8 :: 2 :: 3 :: Nil
val prog: Program = (
    Push(2) ::
    Push(5) :: Push(-3) :: Minus :: Mul ::
    Push(4) :: Push(9) :: Add :: Add :: Nil
)

import Interpreter.*
interpProg(Nil, program)

interpProg(stack5, Add :: Pop :: prog)