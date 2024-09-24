package patmat

object EnumContext:
  enum LookupResult:
    case Ok(v: Int)
    case NotFound

  import LookupResult.*
  enum Context:
    case Empty
    case Cons(name: String, value: Int, tail: Context)

  import Context.*

  def empty: Context = Empty

  def cons(name: String, value: Int, rem: Context) = 
    Cons(name, value, rem)

  def lookup(ctx: Context, name: String): LookupResult = ctx match
    case Empty => NotFound
    case Cons(n, v, r) => if n == name then Ok(v) else lookup(r, name)

  def erase(ctx: Context, name: String): Context = ctx match
    case Empty => Empty
    case Cons(n, v, r) => 
      if n == name then erase(r, name) else cons(n, v, erase(r, name))

  def filter(ctx: Context, pred: (String, Int) => Boolean): Context = ctx match
    case Empty => Empty
    case Cons(n, v, r) =>
      if pred(n, v) then cons(n, v, filter(r, pred)) else filter(r, pred)
