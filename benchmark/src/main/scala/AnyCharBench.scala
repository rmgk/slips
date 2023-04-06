package de.slips.benchmark

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

import de.rmgk.scip.*

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class AnyCharBench {

  inline def anyStringParse = "abcde".any
  inline def oredParse      = ("a".any or "b".any or "c".any or "d".any or "e".any)
  inline def nonMacroParse  = alt("abcde")

  val testString: String = "abcde".repeat(10000).nn
  val ctx        = Scx(testString)

  @Benchmark
  def macroed() = until(anyStringParse.map(!_)).runInContext(ctx.copy())
  @Benchmark
  def ored() = until(oredParse.map(!_)).runInContext(ctx.copy())
  @Benchmark
  def plainMacrodes() = Scip {
    while
      anyStringParse.run
    do ()
  }.runInContext(ctx.copy())
  @Benchmark
  def nonMacroAlt() = until(nonMacroParse.map(!_)).runInContext(ctx.copy())

  @Benchmark
  def manual() = Scip {
    while
      val b = scx.peek
      (b == 'a' || b == 'b' || b == 'c' || b == 'd' || b == 'e') && scx.next && scx.available(1)
    do ()
  }.runInContext(ctx.copy())

}
