package org.scalalabs.basic.lab04

import org.joda.time.{ Duration, DateTime }
import scala.math._
import language.implicitConversions
import language.higherKinds
/**
 * @author arjan
 *
 * This excercise introduces you to Scala implicit conversion features.
 *
 * Scala has a nice feature that automatically lets you convert types and add methods to an existing class.
 * For instance, it is possible to write "Hello".toList, which yields List(H, e, l, l, o) even though
 * the implementation of the String class does not provide a toList method.
 * This is coined 'library pimping' and is achieved via implicit conversions.
 * In this exercise, you will among other try out some implicit conversions from integers to Joda's DateTime,
 * so we can write little DSL like statements like 1 day + 2 hours.
 *
 * Provide a suitable implementation in order to make the corresponding unittest work.
 *
 * Reference material to solve these exercises can be found here:
 * Implicit conversions: http://programming-scala.labs.oreilly.com/ch08.html#Implicits
 *
 */

 object ImplictConversionExercise01 {

  def stringToList(s: String): List[Char] = s toList

}

/**============================================================================ */

object ImplictConversionExercise02 {
  class Celsius(val degree: Double)
  class Fahrenheit(val fahrenheit: Double)

  object TemperaturPrinter {
    def printCelsius(c: Celsius): String = {
      "It's " + c.degree + " degree celsius"
    }

    def printFahrenheit(f: Fahrenheit): String = {
      "It's " + f.fahrenheit + " fahrenheit"
    }
  }

  /**
   * Use this conversion helper to convert fahrenheit values to degree celsius values
   * and vice versa in the implicit function you will define.
   */
   object ConversionHelper {
     def fahrenheit2CelsiusConversion(fahrenheit: Double) = {
      val converted = (fahrenheit - 32) / 1.8
      round(converted * 100).toDouble / 100
    }

    def celsius2FahrenheitConversion(degreeCelsius: Double) = {
      degreeCelsius * 1.8 + 32
    }

    implicit def f2c(f: Fahrenheit): Celsius = {
      new Celsius(fahrenheit2CelsiusConversion(f.fahrenheit))
    }

    implicit def c2f(c: Celsius): Fahrenheit = {
      new Fahrenheit(celsius2FahrenheitConversion(c.degree))
    }
  }
}

/**============================================================================ */
// Write here an implict class that adds a camelCase method to string.

object ImplictConversionExercise03 {
  implicit class StringToRichString(val s: String) extends AnyVal{  
    def camelCase = {
      val str = s split(' ') map (e => s"${e.head toUpper}${e.tail}") mkString; 
      str.head.toLower + str.tail
    }
  }
}

/**============================================================================ */
object ImplictConversionExercise04 {

  object TimeUtils {
    case class DurationBuilder(timeSpan: Long) {
      def now = new DateTime().getMillis()
      def seconds = (timeSpan - days * (86400L * 1000L) - hours * (3600L * 1000L) - minutes * (60L * 1000L)) / 1000L
      def minutes = (timeSpan - days * (86400L * 1000L) - hours * (3600L * 1000L)) / (60L * 1000L)
      def hours = (timeSpan - days * (86400L * 1000L)) / (3600L * 1000L)
      def days = timeSpan / (86400L * 1000L)
      def millis = timeSpan
      def+(that:DurationBuilder) = DurationBuilder(millis + that.millis)
      override def toString = s"$days days, $hours hours, $minutes minutes, $seconds seconds"
    }

    //TODO define some implicits that convert integers and longs to durations and builders to make it all work

    def seconds(in: Long) = in * 1000L
    def minutes(in: Long) = seconds(in) * 60L
    def hours(in: Long) = minutes(in) * 60L
    def days(in: Long) = hours(in) * 24L

    implicit class DurationUtils(n: Int) {
      def seconds = DurationBuilder(n * 1000L)
      def minutes = DurationBuilder(60L * n * 1000L)
      def hours = DurationBuilder(60L * 60L * n * 1000L)
      def days = DurationBuilder(24L * 60L * 60L * n * 1000L)
    }
  }

  case class RichDuration(val duration: Duration) {
    def millis = duration.getMillis()

    def afterNow = new DateTime().plus(duration)

    def +(that: RichDuration) = RichDuration(this.duration.plus(that.duration))
  }
}
/**
 * Create a money DSL which allows you to create Euro classes as follows:
 * - 2 euros           => Euro(2, 0)
 * - 40 cents          => Euro(0, 40)
 * - 2 euros 45 cents  => Euro(2,45)
 * The Euro case class is already provided.
 * Hint: Use an intermediate class (e.g. EuroBuilder) to create the Euro object.
 * E.g. 2 euros = 2 -> EuroBuilder
 * Use an implicit conversion from EuroBuilder -> Euro to get the final result
 * In the EuroBuilder you might need the apply() method to cover this case:
 * 2 euros >45< cents
 */
 object ImplictConversionExercise05 {
  case class Euro(val euros: Int, val cents: Int) 
  
  object Euro {
    def fromCents(cents: Int) = new Euro(cents / 100, cents % 100)
  }

  // object Conversions{
  //   case class EuroBuilder(e: Int, c: Int) {
  //     def apply(n: Int) = EuroBuilder(this.e, n)
  //     def cents = Euro(this.e, this.c)
  //     implicit def toEuro(eb: EuroBuilder) = Euro(this.e, this.c)
  //   }

  //   implicit class Int2EuroBuilder(e: Int) {
  //     def euros = EuroBuilder(e, 0)
  //     def cents = Euro.fromCents(e)
  //   }
  // }
  case class EuroBuilder(val amount: Int, val inCents: Int) {
    def euros = new EuroBuilder(0, inCents + amount * 100)
    def cents = new EuroBuilder(0, inCents + amount)
    def apply(amount: Int) = new EuroBuilder(0, inCents + amount)
  }

  implicit def fromEuroBuilder(eb: EuroBuilder):Euro = Euro.fromCents(eb.inCents)
  implicit def fromInt(value: Int):EuroBuilder= new EuroBuilder(value, 0)
}
