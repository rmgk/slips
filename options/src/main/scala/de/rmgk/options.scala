package de.rmgk

import scopt.*

import scala.annotation.targetName
import scala.collection.mutable.ListBuffer
import scala.compiletime.{constValue, erasedValue, summonAll, summonInline}
import scala.deriving.Mirror
import scala.reflect.ClassTag

object options {
  enum Style:
    case Named()
    case Positional()

  type Single[T] = T
  type Flag[T]   = Boolean

  class Argument[T, Occurrences[_], OptStyle <: Style](
      private[options] val transform: OParser[T, Any] => OParser[T, Any] = identity[OParser[T, Any]],
      private[options] val default: Option[Occurrences[T]] = None
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

  class Subcommand[T <: Product](
      inner: T,
      private[options] val transform: OParser[Unit, Any] => OParser[Unit, Any],
      private[options] val subparser: OParser[_, Any]
  ) {
    private[options] var isUsed: Boolean = false

    def value: Option[T] = Option.when(isUsed)(inner)
  }

  object Subcommand {
    inline def apply[T <: Product](
        inner: T,
        transform: OParser[Unit, Any] => OParser[Unit, Any] = identity
    )(
        using pm: Mirror.ProductOf[T]
    ): Subcommand[T] = {
      val childParser =
        makeParserRec[pm.MirroredElemTypes, pm.MirroredElemLabels, T](inner, OParser.builder[Any], None, 0)
      new Subcommand[T](inner, transform, childParser.asInstanceOf)
    }
  }

  private inline def makeParserRec[ArgsT <: Tuple, Labels, Args <: Product](
      instance: Args,
      builder: scopt.OParserBuilder[Any],
      acc: Option[scopt.OParser[_, Any]],
      position: Int
  ): scopt.OParser[_, Args] =

    def getCurrent = instance.productElement(position)

    inline (erasedValue[ArgsT], erasedValue[Labels]) match
      case (EmptyTuple, EmptyTuple) => acc.get.asInstanceOf[scopt.OParser[_, Args]]
      case (_: (Subcommand[τ] *: rargs), (_: (label *: rlabels))) =>
        val subvalue = getCurrent.asInstanceOf[Subcommand[τ]]

        val childParser = subvalue.subparser

        val name = constValue[label].asInstanceOf[String]

        val end = subvalue.transform(builder.cmd(name).action { (_, args) =>
          val inst = subvalue
          inst.isUsed = true
          args
        }.children(childParser))


        val nextAcc = acc.fold(end)(a => a ++ end)

        makeParserRec[rargs, rlabels, Args](instance, builder, Some(nextAcc), position + 1)

      case (_: (Argument[τ, occ, sty] *: rargs), (_: (label *: rlabels))) =>
        def currentT() = getCurrent.asInstanceOf[Argument[τ, occ, sty]]

        val name = constValue[label].asInstanceOf[String]
        val start: scopt.OParser[τ, Any] = inline erasedValue[sty] match
          case _: Style.Named      => builder.opt[τ](name)(using summonInline[Read[τ]])
          case _: Style.Positional => builder.arg[τ](s"<$name>")(using summonInline[Read[τ]])

        val middle = start.action { (c, args) =>
          currentT().collectValue(c)
          args
        }.valueName(s"<${summonInline[ClassTag[τ]].runtimeClass.getSimpleName}>")

        val validating = inline erasedValue[occ[τ]] match
          case _: List[τ]   => middle.unbounded()
          case _: Option[τ] => middle.optional()
          case _: Flag[τ]   => middle.optional()
          case _: Single[τ] => if currentT().default.isEmpty then middle.minOccurs(1) else middle

        val end = currentT().transform(validating)

        val nextAcc = acc.fold(end)(a => a ++ end)

        makeParserRec[rargs, rlabels, Args](instance, builder, Some(nextAcc), position + 1)

  inline def makeParser[Args <: Product](instance: Args, init: OParserBuilder[Args] => OParser[Unit, Args] = null)(using pm: Mirror.ProductOf[Args]) =
    val builder = scopt.OParser.builder[Args]
    makeParserRec[pm.MirroredElemTypes, pm.MirroredElemLabels, Args](
      instance,
      builder.asInstanceOf,
      Option(init).map(f => f(builder).asInstanceOf[OParser[_, Any]]),
      0
    )

}
