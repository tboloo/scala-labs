package org.scalalabs.basic.lab01

import java.lang.{ IllegalArgumentException => IAE }
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class OOExerciseTest extends Specification {

  "Exercise 1: Creating an Euro" should {
    "be initialized correctly" in {
     val e = new Euro(1, 5)
     e.euro ==== 1
     e.cents ==== 5

   }
   "have correct default value for cents" in {
     val e = new Euro(2)
     e.euro ==== 2
     e.cents ==== 0
   }
   "convert amount correctly to cents" in {
     val e = new Euro(2, 45)
     e.inCents ==== 245
   }
   "be created by cents" in {
     val e = Euro.fromCents(245)
     e.euro ==== 2
     e.cents ==== 45
   }
   "add another euro correctly" in {
     val res = new Euro(1, 50) + new Euro(2, 70)
     res.euro ==== 4
     res.cents ==== 20
   }
   "multiply correctly by a factor" in {
     val res = new Euro(1, 50) * 3
     res.euro ==== 4
     res.cents ==== 50
   }
 }
 "Exercise 2: an Euro" should {
  "be a subclass of Currency" in {
   val e: Currency = new Euro(2, 5)
   e.symbol ==== "EUR"
 }
 "have correct toString representation" in {
   val e = new Euro(2, 5)
   e.toString ==== "EUR: 2,05"
   val e2 = new Euro(2)
   e2.toString ==== "EUR: 2,--"
 }
}
"Exercise 3: Euro with Order trait" should {
  "be correctly orderable" in {
   val e1 = new Euro(1, 5)
   val e2 = new Euro(4, 53)
   val e3 = new Euro(8, 10)
   e3 > e1 must beTrue
   val list = List(e2, e1, e3)
   list.sorted ==== List(e1, e2, e3)
 }
}

"Exercise 4: Implicit class" should {
  "add *(euro:Euro) (multiply) method to Int" in {
   import Euro._
   import ImplicitConverter._
   val res = 3 * new Euro(2, 50)
   res.euro ==== 7
   res.cents ==== 50
 }
 
 "implicitly convert from euro to dollar" in {
  import Dollar._
   val e: Euro = new Dollar(1, 5)
   e.euro ==== 1
   e.cents ==== 42
 }
}
}
