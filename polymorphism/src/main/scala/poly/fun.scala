package poly

import poly.MyList.*



def andLifter[A](f: A => Boolean, g: A => Boolean): A => Boolean =
  a => f(a) && g(a)

def orLifter[A](f: A => Boolean, g: A => Boolean): A => Boolean =
  a => f(a) || g(a)
def sumLifter[A](f: A => Int, g: A => Int): A => Int =
  a => f(a) + g(a)
def listConcatLifter[A, B](f: A => MyList[B], g: A => MyList[B]): A => MyList[B] =
  a => f(a) ++ g(a)

def binaryLifter[A, B, C](f: A => B, g: A => B)(op: (B, B) => C): A => C =
  ???

def andLifter1[A](f: A => Boolean, g: A => Boolean) =
  ???

def orLifter1[A](f: A => Boolean, g: A => Boolean) =
  ???

def sumLifter1[A](f: A => Int, g: A => Int) =
  ???

def listConcatLifter1[A, B](f: A => MyList[B], g: A => MyList[B]) =
  ???
