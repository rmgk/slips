package de.rmgk

import scopt.Read

import scala.annotation.targetName
import scala.collection.mutable.ListBuffer
import scala.compiletime.summonAll
import scala.compiletime.summonInline
import scala.compiletime.erasedValue
import scala.compiletime.constValue
import scala.deriving.Mirror

object options {
  enum OptStyle:
    case Named()
    case Positional()

  type Required[T] = T

  case class Argument[T, Occurrences[_], ArgName <: String, Description <: String, Style <: OptStyle](default: Option[Occurrences[T]] = None) {
    private val contents: ListBuffer[T]           = ListBuffer.empty
    private[options] def collectValue(v: T): Unit = contents.addOne(v)

    inline def value: Occurrences[T] =
      if contents.isEmpty && default.nonEmpty then default.get
      else
        inline erasedValue[Occurrences[T]] match
        case _: List[_]     => contents.toList.asInstanceOf[Occurrences[T]]
        case _: Option[_]   => contents.headOption.asInstanceOf[Occurrences[T]]
        case _: Required[_] => contents.head.asInstanceOf[Occurrences[T]]

  }

  inline def makeParserRec[ArgsT <: Tuple, Labels, Args <: Product](
      builder: scopt.OParserBuilder[Args],
      acc: scopt.OParser[_, Args],
      position: Int
  ): scopt.OParser[_, Args] =
    inline (erasedValue[ArgsT], erasedValue[Labels]) match
      case (EmptyTuple, EmptyTuple) => acc
      case (_: (Argument[τ, occ, argname, desc, sty] *: rargs), (_: (label *: rlabels))) =>
        val desc    = constValue[desc]
        val name    = constValue[label].asInstanceOf[String]
        val argname = constValue[argname]
        val start: scopt.OParser[τ, Args] = inline erasedValue[sty] match
          case _: OptStyle.Named      => builder.opt[τ](name)(using summonInline[Read[τ]])
          case _: OptStyle.Positional => builder.arg[τ](name)(using summonInline[Read[τ]])
        val middle = inline erasedValue[occ[Any]] match
          case _: List[_]     => start.unbounded()
          case _: Option[_]   => start.optional()
          case _: Required[_] => start.required()

        val end = middle.action { (c, args) =>
          args.productElement(position).asInstanceOf[Argument[τ, occ, argname, desc, sty]].collectValue(c)
          args
        }.text(desc).valueName(argname)

        makeParserRec[rargs, rlabels, Args](builder, end.flatMap(_ => acc), position + 1)

  inline def makeParser[Args <: Product](name: String)(using pm: Mirror.ProductOf[Args]) =
    val builder = scopt.OParser.builder[Args]
    makeParserRec[pm.MirroredElemTypes, pm.MirroredElemLabels, Args](builder, builder.programName(name), 0)

}
