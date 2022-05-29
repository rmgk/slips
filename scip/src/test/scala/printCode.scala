import scala.quoted.*

inline def printCode[T](inline expr: T, inline ct: Boolean = false): String =
  ${ impl('expr, 'ct) }

def impl[T: Type](expr: Expr[T], ct: Expr[Boolean])(using quotes: Quotes): Expr[String] = {

  import quotes.reflect.*
  val e = expr.show.toString
  // val t = expr.asTerm.toString
  if (ct.value.getOrElse(false)) println(e)
  // println(t)
  Expr(e)

}
