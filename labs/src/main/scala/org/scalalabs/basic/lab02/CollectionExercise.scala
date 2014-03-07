package org.scalalabs.basic.lab02
/**
 * This Lab contains exercises where the usage of
 * higher order collection methods can be rehearsed.
 */
 import sys._

 object CollectionExercise01 {

  /**
   * Taken from: <a href="http://code.google.com/codejam/contest/1460488/dashboard">Problem A. Speaking in Tongues</a>
   *
   * Problem
   * The aim of this task is to translate a language into a new language called Googlerese.
   * To translate we take any message and replace each English letter with another English letter.
   * This mapping is one-to-one and onto, which means that the same input letter always gets replaced
   * with the same output letter, and different input letters always get replaced with different output letters.
   * A letter may be replaced by itself. Spaces are left as-is.
   *
   * For example (and here is a hint!), the translation algorithm includes the following three mappings:
   * 'a' -> 'y', 'o' -> 'e', and 'z' -> 'q'. This means that "a zoo" will become "y qee".
   *
   * Sample Input/Output
   * Input:
   * Case 1: ejp mysljylc kd kxveddknmc re jsicpdrysi
   * Case 2: rbcpc ypc rtcsra dkh wyfrepkym veddknkmkrkcd
   * Case 3: de kr kd eoya kw aej tysr re ujdr lkgc jv
   *
   * Output:
   * Case 1: our language is impossible to understand
   * Case 2: there are twenty six factorial possibilities
   * Case 3: so it is okay if you want to just give up
   *
   */
   def googleCodeJamGooglerese(lines: String*): Seq[String] = {
    // "ejp mysljylc kd kxveddknmc re jsicpdrysi" split(' ') mkString zip "our language is impossible to understand" split(' ') mkString toMap map (_.swap)
    val decode = Map(' ' -> ' ' , 'a'-> 'y', 'b' -> 'h', 'c' -> 'e', 'd' -> 's', 'e' -> 'o', 'f' -> 'c', 'g' -> 'v', 'h' -> 'x', 'i' -> 'd', 'j' -> 'u', 'k' -> 'i', 'l' -> 'g', 'm' -> 'l', 'n' -> 'b', 'o' -> 'k', 'p' -> 'r', 'r' -> 't', 's' -> 'n', 't' -> 'w', 'u' -> 'j', 'v' -> 'p', 'w' -> 'f', 'x' -> 'm', 'y' -> 'a')
    for {
      l <- lines
    } yield l map (c => decode(c))
  }
}
/*========================================================== */

object CollectionExercise02 {

  class Person(val age: Int, val name: String) {
    override def toString = s"$name, $age"
  }

  /**
   * Take a look at the java class: {@link ImperativeSample}. The
   * groupAdultsPerAgeGroup is implemented using an imperative programming
   * style.
   * Rewrite the method groupAdultsPerAgeGroup in the ImperativeSample java class
   * using a functional approach.
   */
   def groupAdultsPerAgeGroup(persons: Seq[Person]): Map[Int, Seq[Person]] = {
    (persons filter (_.age >= 20) groupBy (_.age/10*10)) map (e => (e._1, e._2.sortBy(_.name)))
  }
}

/*========================================================== */

object CollectionExercise03 {
  /**
   * Create a method that checks that each subsequent value is greater than
   * the previous one.
   * E.g.:
   * checkValuesIncrease(Seq(1,2,3)) == true
   * checkValuesIncrease(Seq(1,2,2)) == false
   */
   def checkValuesIncrease[T <% Ordered[T]](seq: Seq[T]): Boolean = ((seq distinct) sorted) == seq

 }
 /*========================================================== */

 object CollectionExercise04 {
  /**
   * Calculate the length of the longest word in a list of sentences.
   * To keep it simple it's ok to use String.split to extract all words of a sentence.
   */
   def calcLengthLongestWord(lines: String*): Int = {
    val longest = 
    for { 
      l <- lines 
      val top = (l split(' ') map (w => w.length) sorted) last
      } yield top
      (longest sorted) last
    }
  }




