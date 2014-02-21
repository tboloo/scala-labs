package org.scalalabs.basic.lab01
import scala.language.implicitConversions

/**
 * The goal of this exercise is to get familiar basic OO constructs in scala
 *
 * Fix the code so that the unit test 'CurrencyExerciseTest' passes.
 *
 * In order for the tests to pass you need to do the following:
 * 
 * Exercise 1:
 * - Create a class Euro
 * - Provide it with two constructor parameters: euro:Int, cents:Int
 * - Provide the cents field with default value: 0
 * - Provide an immutable field named: inCents that converts euro + cents into cents.
 * - Create an object Euro with a factory method named: fromCents that creates an Euro based on cents.
 * - Create a method named: + to the Euro class that adds another Euro
 * - Create a method named: * to the Euro class that multiplies an Euro
 * 
 * Exercise 2:
 * - Create an abstract class Currency
 * - Provide it with one constructor parameter: symbol:String
 * - Extend the previously created Euro class from Currency
 * - Override the toString method of Euro to represent the following String:
 *   -> symbol + ': ' + euro + ',' + cents.  E.g: EUR 200,05
 * - In case the cents are 0 use this representation:
 *   -> symbol + ': ' + euro + ',--. E.g.: EUR 200.--
 *   
 * Exercise 3:
 * - Mix the Ordered trait in Euro
 * - Implement the compare method  
 * 
 * Exercise 4:
 * - Provide an implicit class that adds a *(euro:Euro) method to Euro
 * - Create a new currency Dollar
 * - Provide a conversion method that converts from Euro to Dollar
 */
 class Euro(val euro:Int, val cents: Int = 0) extends Currency("EUR") with Ordered[Euro] {
 	def inCents = euro*100 + cents
 	def +(other: Euro) = Euro.fromCents(inCents + other.inCents)
 	def *(times: Int) = Euro.fromCents(inCents*times)
 	override def toString = cents match {
 		case 0 => s"$symbol: $euro,--"
 		case _ => f"$symbol: $euro,$cents%02d"
 	}
 	def compare(that : Euro) : Int = inCents - that.inCents
 }

 object Euro {
 	def fromCents(cents: Int) = new Euro(cents / 100, cents % 100)
 }

 abstract class Currency(val symbol: String)

 object ImplicitConverter {
 	implicit class IntToEuro(val n: Int) extends AnyVal{  
 		def *(euro: Euro) = euro * n
 	}	
 }

 class Dollar(val dollars: Int, val cents: Int) extends Currency("USD") with Ordered[Dollar] {
 	def inCents = dollars*100 + cents
 	def compare(that : Dollar) : Int = inCents - that.inCents
 }

 object Dollar {
 	implicit def toEuro(d: Dollar): Euro = {
 		Euro.fromCents(d.inCents / 0.7394366197183099 toInt)
 	} 
 }
