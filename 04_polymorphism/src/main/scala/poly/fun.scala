package poly

import poly.MyList.*

/**
  * Composes the to functions f and g to f∘g
  *
  * @param f function of type B => C
  * @param g function of type A => B
  * @return  function f∘g of type A => C
  */
def compose[A, B, C](f: B => C) (g: A => B): A => C =
  (x: A) => f(g(x))

def andLifter[A](f: A => Boolean, g: A => Boolean): A => Boolean =
  a => f(a) && g(a)

def orLifter[A](f: A => Boolean, g: A => Boolean): A => Boolean =
  a => f(a) || g(a)

def sumLifter[A](f: A => Int, g: A => Int): A => Int =
  a => f(a) + g(a)

def listConcatLifter[A, B](f: A => MyList[B], g: A => MyList[B]): A => MyList[B] =
  a => f(a) ++ g(a)

def binaryLifter[A, B, C](f: A => B, g: A => B)(op: (B, B) => C): A => C =
  a => op(f(a), g(a))

def andLifter1[A](f: A => Boolean, g: A => Boolean) =
  binaryLifter(f, g)((_ && _))

def orLifter1[A](f: A => Boolean, g: A => Boolean) =
  binaryLifter(f, g)((_ || _))

def sumLifter1[A](f: A => Int, g: A => Int) =
  binaryLifter(f, g)((_ + _))

def listConcatLifter1[A, B](f: A => MyList[B], g: A => MyList[B]) =
  binaryLifter(f, g)((_ ++ _))