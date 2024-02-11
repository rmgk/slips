import de.rmgk.scip.*

import java.nio.charset.StandardCharsets

object Json {
// this was ported from https://github.com/com-lihaoyi/fastparse/blob/master/perftests/bench1/src/perftests/JsonParse.scala
// which is MIT licensed:
  /*
The MIT License (MIT)

Copyright (c) 2014 Li Haoyi (haoyi.sg@gmail.com)

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   */

  object Js {
    sealed trait Val extends Any {
      def value: Any
      def apply(i: Int): Val = this.asInstanceOf[Arr].value(i)
      def apply(s: java.lang.String): Val =
        this.asInstanceOf[Obj].value.find(_._1 == s).get._2
    }
    case class Str(value: java.lang.String)         extends AnyVal with Val
    case class Obj(value: (java.lang.String, Val)*) extends AnyVal with Val
    case class Arr(value: Val*)                     extends AnyVal with Val
    case class Num(value: Double)                   extends AnyVal with Val
    case object False extends Val {
      def value = false
    }
    case object True extends Val {
      def value = true
    }
    case object Null extends Val {
      def value = null
    }
  }

  inline def space: Scip[Boolean]      = " \t\r\n".any.rep.min(0)
  inline def digits: Scip[Int]         = "0123456789".any.rep
  inline def exponent: Scip[Boolean]   = "eE".any and "+-".any.opt and digits.min(1)
  inline def fractional: Scip[Boolean] = ".".any and digits.min(1)
  inline def integral: Scip[Boolean]   = "0".any or ("123456789".any and digits.min(0))

  inline def number = ("+-".any.opt and integral and fractional.opt and exponent.opt).str.map(x => Js.Num(x.toDouble))

  inline def `null`  = "null".all.orFail.map(_ => Js.Null)
  inline def `false` = "false".all.orFail.map(_ => Js.False)
  inline def `true`  = "true".all.orFail.map(_ => Js.True)

  inline def hexDigit      = "0123456789abcdefABCDEF".any
  inline def unicodeEscape = "u".all and hexDigit and hexDigit and hexDigit and hexDigit
  inline def escape        = "\\".all and ("\"/\\bfnrt".any or unicodeEscape)

  inline def strChars = until("\"\\".any).min(1)
  inline def string =
    ((space and "\"".all).orFail ~> (strChars or escape).rep.min(0).str <~ "\"".all.orFail).map(Js.Str.apply)

  inline def array =
    ("[".all.orFail ~> jsonExpr.list(sep = ",".all) <~ (space and "]".all).orFail).map(Js.Arr(_*))

  inline def pair = (string.map(_.value).trace("key") <~> (":".all.orFail ~> jsonExpr)).trace("pair")

  inline def obj: Scip[Js.Obj] =
    ("{".all.orFail ~> pair.list(",".all) <~ (space and "}".all).orFail).map(Js.Obj(_*))

  // to break recursive inlining :-)
  def jsonExpr: Scip[Js.Val] = iJsonExpr

  inline def iJsonExpr: Scip[Js.Val] =
    space.orFail ~> (obj.trace("obj") | array.trace("array") | string.trace("string") | `true`.trace(
      "true"
    ) | `false`.trace("false") | `null`.trace("null") | number.trace("number")).trace("expr") <~ space.orFail

}

class JsonTest extends munit.FunSuite {

  val mystr = """{
        |  "firstName": "John",
        |  "lastName": "Smith",
        |  "age": 25,
        |  "address": {
        |      "streetAddress": "21 2nd Street",
        |      "city": "New York",
        |      "state": "NY",
        |      "postalCode": 10021
        |  },
        |  "phoneNumbers": [
        |      {
        |          "type": "home",
        |          "number": "212 555-1234"
        |      },
        |      {
        |          "type": "fax",
        |          "number": "646 555-4567"
        |      }
        |  ]
        |}""".stripMargin

  test("") {

    import Json.Js.*

    assertEquals(
      Json.jsonExpr.runInContext(Scx(mystr).copy(tracing = false)),
      Obj(
        ("firstName", Str("John")),
        ("lastName", Str("Smith")),
        ("age", Num(25.0)),
        (
          "address",
          Obj(
            ("streetAddress", Str("21 2nd Street")),
            ("city", Str("New York")),
            ("state", Str("NY")),
            ("postalCode", Num(10021.0))
          )
        ),
        (
          "phoneNumbers",
          Arr(
            Obj(("type", Str("home")), ("number", Str("212 555-1234"))),
            Obj(("type", Str("fax")), ("number", Str("646 555-4567")))
          )
        )
      )
    )
  }

}
