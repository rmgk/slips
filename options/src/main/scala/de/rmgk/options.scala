package de.rmgk

import de.rmgk.options.ArgumentContext.{ParseException, ParseResult}
import de.rmgk.resource.{Resource, ResourceContext, collectResources}

import java.nio.file.Path
import scala.reflect.ClassTag

object options:

  @FunctionalInterface
  trait ArgumentParser[T]:
    def apply(str: String): Option[T]
  object ArgumentParser:
    given ArgumentParser[Path]                                       = s => Some(Path.of(s).nn)
    given ArgumentParser[String]                                     = Some.apply
    given ArgumentParser[Int]                                        = _.toIntOption
    given ArgumentParser[Long]                                       = _.toLongOption
    given ArgumentParser[Double]                                     = _.toDoubleOption
    given ArgumentParser[Boolean]                                    = _ => Some(true)
    given [T](using p: ArgumentParser[T]): ArgumentParser[Option[T]] = s => p.apply(s).map(Some.apply)

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
      val parser: ArgumentParser[T],
      val ct: ClassTag[T]
  ) extends Resource {
    type Type = T
  }

  class RemainingArguments(name: String, description: String = "")
      extends Argument[List[String]](name, Style.Positional, description = description, default = Nil)(using
        _ => None,
        summon
      )

  inline def parseArguments[Res](parameters: List[String])(inline expr: Res): ParseResult[Res] =
    val (descriptors, handler) =
      collectResources[Res, de.rmgk.options.Argument[_], de.rmgk.options.ArgumentContext](expr)
    executeParsing(parameters, descriptors, handler)

  def executeParsing[Res](
      parameters: List[String],
      descriptors: List[Argument[_]],
      handler: ArgumentContext => Res
  ): ParseResult[Res] =
    ArgumentContext.exec(parameters, descriptors, handler)

  case class ArgumentContext(bound: Map[Argument[_], Any]) extends ResourceContext[Argument[_]] {
    override def accessResource(res: Argument[_]): res.Type =
      bound.get(res).orElse(Option(res.default)).map(_.asInstanceOf[res.Type]).getOrElse:
        throw ParseException(s"required argument »${res.name}« not provided")
  }

  object ArgumentContext {

    def exec[Res](
        parameters: List[String],
        descriptors: List[Argument[_]],
        handler: ArgumentContext => Res
    ): ParseResult[Res] =
      try
        val bound = ArgumentContext.rec(parameters, descriptors, Map.empty)
        ParseResult(Right(handler(ArgumentContext(bound))))
      catch
        case ParseException(msg) =>
          ParseResult(Left(ParseError(descriptors, msg)))

    case class ParseResult[T](inner: Either[ParseError, T]):
      def printHelp(): Unit =
        inner match
          case Left(pe) =>
            println("commandline arguments:")
            println(pe.formatHelp.indent(2))
            println(s"\n  Note: ${pe.msg}")
          case Right(value) => ()

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
                        arg.parser(argstr) match
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
                  rec(
                    rest,
                    descriptors,
                    bound.updatedWith(arg): (curr: Option[Any]) =>
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
