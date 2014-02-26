package org.scalalabs.basic.lab03
import scala.language.reflectiveCalls
import sys._
/**
 * This exercise introduces you to Scala functions.
 *
 * Functions let you separate responsibilities, which allow you to maximally reuse code.
 *
 * Create a method measure that accepts any code blocks, executes it and prints the execution time.
 * E.g. 'The execution took <elapsed> ms'.
 * Use the logPerf method provided.
 * Provide a suitable implementation in order to make the corresponding unittest work.
 */
 object FunctionsExercise01 {

  var printed = "" 
  private def logPerf(elapsed: Long) = printed = s"The execution took: $elapsed ms"

  def measure[T](block: => T): T = {
    val start = System.nanoTime
    val res = block
    val elapsed = (System.nanoTime - start) / 1000
    printed =  s"The execution took: $elapsed ms"
    res
  }

}

/**
 * Functions let you create control abstractions, which give extra opportunities to condense
 * and simplify code.
 *
 * Provide a suitable implementation in order to make the corresponding unittest work.
 */
 object FunctionsExercise02 {

  def plusOne(x: Int): Int = {
    //implement this using a partial function
    val sum = (a:Int, b:Int) => a + b
    val inc = sum(_:Int, 1)
    inc(x)
  }

  def plus(x: Int, y: Int): Int = {
    x + y
  }

  def using[A <: { def close(): Unit }, B](closable: A)(f: A => B): B = {
    try { 
      f(closable)
      } finally {
        closable.close
      }
    }
  }
