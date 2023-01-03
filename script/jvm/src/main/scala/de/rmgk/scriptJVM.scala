package de.rmgk.script

import de.rmgk.delay.{Async, toAsync}

import java.lang.ProcessBuilder.Redirect
import scala.util.Using

import scala.language.unsafeNulls

extension (pb: ProcessBuilder)
  def asyncResult: Async[Any, Either[Int, String]] = Async {
    val process = pb
      .inheritIO().redirectOutput(Redirect.PIPE)
      .start().onExit().toAsync.bind
    val code = process.exitValue()

    if code != 0
    then Left(code)
    else Right(Using(process.getInputStream)(streamToString).get)
  }
