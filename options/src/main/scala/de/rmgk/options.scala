package de.rmgk

import scopt.*

import scala.annotation.targetName
import scala.collection.mutable.ListBuffer
import scala.compiletime.{constValue, erasedValue, summonAll, summonInline}
import scala.deriving.Mirror

object options {
  enum Style:
    case Named()
    case Positional()

  type Single[T] = T
  type Flag[T]   = Boolean

  case class Argument[T, Occurrences[_], OptStyle <: Style](
      transform: OParser[T, Any] => OParser[T, Any] = identity,
      default: Option[Occurrences[T]] = None
  ) {
    private[options] val contents: ListBuffer[T]  = ListBuffer.empty
    private[options] def collectValue(v: T): Unit = contents.addOne(v)

    inline def value: Occurrences[T] =
      if contents.isEmpty && default.nonEmpty then default.get
      else
        inline erasedValue[Occurrences[T]] match
          case _: List[_]   => contents.toList.asInstanceOf[Occurrences[T]]
          case _: Option[_] => contents.headOption.asInstanceOf[Occurrences[T]]
          case _: Flag[_]   => contents.nonEmpty.asInstanceOf[Occurrences[T]]
          case _: Single[_] => contents.head.asInstanceOf[Occurrences[T]]

  }

  inline def makeParserRec[ArgsT <: Tuple, Labels, Args <: Product](
      instance: Args,
      builder: scopt.OParserBuilder[Args],
      acc: scopt.OParser[_, Args],
      position: Int
  ): scopt.OParser[_, Args] =
    inline (erasedValue[ArgsT], erasedValue[Labels]) match
      case (EmptyTuple, EmptyTuple) => acc
      case (_: (Argument[τ, occ, sty] *: rargs), (_: (label *: rlabels))) =>
        def getArg(args: Args) = args.productElement(position).asInstanceOf[Argument[τ, occ, sty]]

        val name = constValue[label].asInstanceOf[String]
        val start: scopt.OParser[τ, Args] = inline erasedValue[sty] match
          case _: Style.Named      => builder.opt[τ](name)(using summonInline[Read[τ]])
          case _: Style.Positional => builder.arg[τ](s"<$name>")(using summonInline[Read[τ]])

        val middle = start.action { (c, args) =>
          getArg(args).collectValue(c)
          args
        }

        val validating = inline erasedValue[occ[τ]] match
          case _: List[τ]   => middle.unbounded()
          case _: Option[τ] => middle.optional()
          case _: Flag[τ]   => middle.optional()
          case _: Single[τ] => if getArg(instance).default.isEmpty then middle.minOccurs(1) else middle

        val end = getArg(instance).transform(validating.asInstanceOf).asInstanceOf[OParser[_, Args]]

        makeParserRec[rargs, rlabels, Args](instance, builder, end.flatMap(_ => acc), position + 1)

  inline def makeParser[Args <: Product](name: String, instance: Args)(using pm: Mirror.ProductOf[Args]) =
    val builder = scopt.OParser.builder[Args]
    makeParserRec[pm.MirroredElemTypes, pm.MirroredElemLabels, Args](instance, builder, builder.programName(name), 0)

}
