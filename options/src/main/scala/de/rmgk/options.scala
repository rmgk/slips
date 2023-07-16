package de.rmgk

import de.rmgk.options.ParsedArguments.{ParseError, ParseException}
import de.rmgk.resources.{Resource, ResourceContext, collectResources}

import java.nio.file.Path
import scala.reflect.ClassTag

object options:

  @FunctionalInterface
  trait ArgumentParsers[T]:
    def apply(str: String): Option[T]
  object ArgumentParsers:
    given ArgumentParsers[Path]                                        = s => Some(Path.of(s).nn)
    given ArgumentParsers[String]                                      = Some.apply
    given ArgumentParsers[Int]                                         = _.toIntOption
    given ArgumentParsers[Long]                                        = _.toLongOption
    given ArgumentParsers[Double]                                      = _.toDoubleOption
    given [T](using p: ArgumentParsers[T]): ArgumentParsers[Option[T]] = s => p.apply(s).map(Some.apply)

  enum Style:
    case Named
    case Positional
    case Flag

  case class Argument[T](
      name: String,
      style: Style,
      description: String = "",
      default: T | Null = null
  )(using
      val parser: ArgumentParsers[T],
      val ct: ClassTag[T]
  ) extends Resource {
    type Type = T
  }

  class RemainingArguments(name: String, description: String = "")
      extends Argument[List[String]](name, Style.Positional, description = description, default = Nil)(using _ => None)

  inline def parseArguments[Res](parameters: List[String])(inline expr: Res): Either[ParseError, Res] =
    val (descriptors, handler) = collectResources[Res, Argument[_], ParsedArguments](expr)
    try
      val bound = ParsedArguments.rec(parameters, descriptors, Map.empty)
      Right(handler(ParsedArguments(bound)))
    catch
      case ParseException(msg) =>
        Left(ParseError(descriptors, msg))

  class ParsedArguments(bound: Map[Argument[_], Any]) extends ResourceContext[Argument[_]] {
    override def accessResource(res: Argument[_]): res.Type =
      bound.get(res).orElse(Option(res.default)).map(_.asInstanceOf[res.Type]).getOrElse:
        throw ParseException(s"required argument »${res.name}« not provided")
  }

  object ParsedArguments {

    case class ParseError(descriptors: List[Argument[_]], msg: String):
      def formatHelp: String =
        val ordered =
          val grouped    = descriptors.groupBy(_.style)
          val named      = (grouped.getOrElse(Style.Named, Nil) ++ grouped.getOrElse(Style.Flag, Nil)).sortBy(_.name)
          val positional = grouped.getOrElse(Style.Positional, Nil)
          named ++ positional

        val lines = ordered.map: desc =>
          desc.style match
            case Style.Named      => s"--${desc.name} ${desc.ct.runtimeClass.getSimpleName}"
            case Style.Positional => s"<${desc.name}: ${desc.ct.runtimeClass.getSimpleName}>"
            case Style.Flag       => s"--${desc.name}"
        val maxline = lines.map(_.size).maxOption.getOrElse(0)
        lines.zip(ordered).map: (l, d) =>
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

    def rec(
        remaining: List[String],
        descriptors: List[Argument[_]],
        bound: Map[Argument[_], Any]
    ): Map[Argument[_], Any] =
      remaining match
        case Nil => bound
        case str :: rest =>
          if str.startsWith("--") then
            descriptors.find(_.name == str.substring(2)) match
              case None => throw ParseException(s"passed unexpected flag $str")
              case Some(arg) =>
                val remdesc = descriptors.filterNot(_ == arg)
                arg.style match
                  case Style.Named =>
                    rest match
                      case Nil => throw ParseException("flag needs argument")
                      case argstr :: rest =>
                        arg.parser(str) match
                          case None => throw ParseException(
                              s"could not parse »$str $argstr« as ${arg.ct.runtimeClass.getSimpleName}"
                            )
                          case Some(v) => rec(rest, remdesc, bound.updated(arg, v))
                  case Style.Positional => throw ParseException(s"${arg.name} should not be positional")
                  case Style.Flag =>
                    rec(rest, remdesc, bound.updated(arg, true))
          else
            descriptors.find(_.style == Style.Positional) match
              case None => throw ParseException(s"no more positional arguments")
              case Some(arg) =>
                if arg.isInstanceOf[RemainingArguments]
                then
                  rec(rest, descriptors, bound.updatedWith(arg): (curr: Option[Any]) =>
                    Some(str :: curr.getOrElse(Nil).asInstanceOf[List[String]])
                  )
                else
                  val remdesc = descriptors.filterNot(_ == arg)
                  arg.parser(str) match
                    case None => throw ParseException(s"could not parse »$str« as ${arg.ct.runtimeClass.getSimpleName}")
                    case Some(v) =>
                      rec(rest, remdesc, bound.updated(arg, v))
  }

end options
