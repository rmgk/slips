package de.rmgk

import de.rmgk.resource.{Resource, ResourceContext, collectResources}

import java.nio.file.Path
import scala.annotation.tailrec
import scala.reflect.ClassTag

object options:

  @FunctionalInterface
  trait ArgumentValueParser[T]:
    def apply(args: List[String]): (Option[T], List[String])
    def valueDescription: String
  object ArgumentValueParser:
    def one[T](p: String => Option[T])(using ct: ClassTag[T]): ArgumentValueParser[T] = new:
      def apply(args: List[String]): (Option[T], List[String]) =
        (p(args.head), args.tail)
      def valueDescription: String = ct.runtimeClass.getSimpleName.nn

    given ArgumentValueParser[Path]   = one(s => Some(Path.of(s).nn))
    given ArgumentValueParser[String] = one(Some.apply)
    given ArgumentValueParser[Int]    = one(_.toIntOption)
    given ArgumentValueParser[Long]   = one(_.toLongOption)
    given ArgumentValueParser[Double] = one(_.toDoubleOption)
    given ArgumentValueParser[Boolean] = new:
      def apply(str: List[String]): (Option[Boolean], List[String]) = (Some(true), str)
      def valueDescription                                          = ""
    given [T: ClassTag](using p: ArgumentValueParser[T]): ArgumentValueParser[Option[T]] = new:
      def apply(args: List[String]): (Option[Option[T]], List[String]) =
        val (value, rest) = p.apply(args)
        (value.map(Some.apply), rest)
      def valueDescription = s"Option(${p.valueDescription})"

    def subcommandParser[T](argumentsParser: ArgumentsParser[T]): ArgumentValueParser[T] = new ArgumentValueParser[T] {
      override def apply(args: List[String]): (Option[T], List[String]) =
        val (value, res) = argumentsParser.parseUnsafe(args)
        (Some(value), res)
      override def valueDescription: String = ???
    }
  end ArgumentValueParser
  def positional[T: ArgumentValueParser](hint: String, description: String, default: T | Null = null): Argument[T] =
    Argument("", s"<$hint>", description, default)
  def named[T: ArgumentValueParser](key: String, description: String, default: T | Null = null): Argument[T] =
    Argument(key, key, description, default)

  case class Argument[T](
      key: String,
      hint: String,
      description: String,
      default: T | Null
  )(using
      val parser: ArgumentValueParser[T]
  ) extends Resource {
    type Type = T
  }

  inline def argumentParser[Res](inline expr: Res): ArgumentsParser[Res] =
    val (descriptors, handler) =
      collectResources[Res, de.rmgk.options.Argument[_], de.rmgk.options.ArgumentContext](expr)
    ArgumentsParser(descriptors, handler)

  inline def parseArguments[Res](parameters: List[String])(inline expr: Res): ParseResult[Res] =
    argumentParser(expr).parse(parameters)

  inline def subprogram[Res](name: String)(inline expr: Res) =
    val parser = argumentParser(expr)
    Argument(name, name, "", null)(using ArgumentValueParser.subcommandParser(parser))

  case class ArgumentsParser[Res](descriptors: List[Argument[_]], handler: ArgumentContext => Res):
    def parse(parameters: List[String]): ParseResult[Res] =
      try
        val (result, remaining) = parseUnsafe(parameters)
        if remaining.isEmpty
        then ParseResult(Right(result))
        else ParseResult(Left(ParseError(descriptors, s"unhandled arguments: $remaining")))
      catch
        case ParseException(msg) =>
          ParseResult(Left(ParseError(descriptors, msg)))

    def parseUnsafe(parameters: List[String]): (Res, List[String]) | Nothing =
      val (bound, rem) = rec(parameters, descriptors, Map.empty)
      (handler(ArgumentContext(bound)), rem)

    @tailrec
    final def rec(
        remaining: List[String],
        descriptors: List[Argument[_]],
        bound: Map[Argument[_], Any]
    ): (Map[Argument[_], Any], List[String]) =
      if descriptors.isEmpty then return (bound, remaining)
      remaining match
        case Nil => (bound, Nil)
        case str :: rest =>
          descriptors.find(_.key == str).orElse(descriptors.find(_.key == "")) match
            case None => throw ParseException(s"unexpected argument: »$str«")
            case Some(arg) =>
              val remdesc        = descriptors.filterNot(_ == arg)
              val (value, rest2) = arg.parser.apply(if arg.key == "" then remaining else rest)
              value match
                case Some(v) => rec(rest2, remdesc, bound.updated(arg, v))
                case None    => throw ParseException(s"could not parse: »$str« ($rest)")
  end ArgumentsParser

  case class ArgumentContext(bound: Map[Argument[_], Any]) extends ResourceContext[Argument[_]] {
    override def accessResource(res: Argument[_]): res.Type =
      bound.get(res).orElse(Option(res.default)).map(_.asInstanceOf[res.Type]).getOrElse:
        throw ParseException(s"required argument »${res.key}« not provided")
  }

  case class ParseResult[T](inner: Either[ParseError, T]):
    def printHelp(): Unit =
      inner match
        case Left(pe) =>
          println("commandline arguments:")
          println(pe.formatHelp.indent(2))
          println(s"Note: ${pe.msg}")
        case Right(value) => ()

  case class ParseError(descriptors: List[Argument[_]], msg: String):
    def formatHelp: String =
      val ordered = descriptors.sortBy(_.key)

      val lines = ordered.map: desc =>
        s"${desc.hint} ${desc.parser.valueDescription}"
      val maxline = lines.map(_.size).maxOption.getOrElse(0)
      lines.lazyZip(ordered).map: (l, d) =>
        val res =
          if d.description.nonEmpty
          then s"%-${maxline}s   %s".format(l, d.description)
          else l
        if d.default == null
        then res
        else s"$res\n  (default: ${d.default})"
      .mkString("\n")
    end formatHelp

  case class ParseException(msg: String) extends Exception(msg, null, false, false)

end options
