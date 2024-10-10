package patmat

object FuncContext:
  import EnumContext.LookupResult
  import LookupResult.*

  type Context =
    String => LookupResult

  def empty: Context =
    (_: String) => NotFound

  def cons(name: String, value: Int, rem: Context): Context =
    (s: String) => if s == name then Ok(value) else lookup(rem, s)    

  def lookup(ctx: Context, name: String): LookupResult = 
    ctx(name)

  def erase(ctx: Context, name: String): Context = 
    (s: String) => if s == name then NotFound else lookup(ctx, s)