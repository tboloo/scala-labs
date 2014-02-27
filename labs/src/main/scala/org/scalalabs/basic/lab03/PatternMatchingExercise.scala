package org.scalalabs.basic.lab03
import sys._
/**
 * This exercise introduces you to the powerful pattern matching features of Scala.
 *
 * Pattern matching can in its essence be compared to Java's 'switch' statement,
 * even though it provides many more possibilites. Whereas the Java switch statmenet
 * lets you 'match' primitive types up to int's, Scala's pattern matching goes much
 * further. Practically everything from all types of objects and Collections
 * can be matched, not forgetting xml and a special type of class called case classes.
 *
 * Pattern matching is also often used in combination with recursive algorithms.
 *
 * For this exercise exclusively use pattern matching constructs in order to make the
 * corresponding unit test work.
 *
 * Reference material to solve these exercises can be found here:
 * Pattern matching in general: http://programming-scala.labs.oreilly.com/ch03.html#PatternMatching
 * Pattern matching in combination with partial functions: http://programming-scala.labs.oreilly.com/ch08.html#PartialFunctions
 */

 object PatternMatchingExerciseBasic {

  /*************************************************************************
   *  pattern matching exercises
   * For expected solution see unittest @PatternMatchingExerciseTest
   *************************************************************************/

   def describeLanguage(s: String) = s match {
    case "Java" | "Smalltalk" => "OOP"
    case "Clojure" | "Haskell" => "Functional"
    case "Scala" => "Hybrid"
    case "C" => "Procedural"
    case "Oz" => "Unknown"
  } 

  def matchOnInputType(in: Any) = in match {
    case s: String => s"A string with length ${s.length}"
    case i: Int => "A positive integer"
    case p: Person => s"A person with name: ${p.name}"
    case s: Seq[Any] if s.size > 10 => "Seq with more than 10 elements"
    case Seq(e1,e2,e3,e4) =>  s"first: $e1, second: $e2, rest: List($e3, $e4)"
    case opt: Some[Any] => "A Scala Option subtype"
    case none: None.type => "A Scala Option subtype"
    case null => "A null value"
    case _ => "Some Scala class"
  }


  def older(p: Person): Option[String] = p match {
    case p if p.age > 30 => Some(p.name)
    case _ => None
  }
  

  /*************************************************************************
   * Pattern matching with partial functions
   * For expected solution see @PatternMatchingExerciseTest
   *************************************************************************/

   val pf1: PartialFunction[String, String] = new PartialFunction[String,String] {
    def apply(s: String) = "scala-labs"
    def isDefinedAt(s: String) = s == "scala-labs" || s == "stuff"
  }

  val pf2: PartialFunction[String, String] = new PartialFunction[String,String] {
    def apply(s: String) = "other stuff"
    def isDefinedAt(s: String) = s == "other stuff"
  }

  val pf3:PartialFunction[String, String] = {
    pf1 orElse pf2
  }

}

case class Person(name: String, age: Int)