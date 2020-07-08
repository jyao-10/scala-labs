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
 * - Provide an implicit class that adds a *(euro:Euro) method to Int
 * - Create a new currency Dollar
 * - Provide a implicit conversion method that converts from Dollar to Euro using the
 *   [[org.scalalabs.basic.lab01.DefaultCurrencyConverter]]
 *
 * Exercise 5:
 * - Extend the conversion method from Dollar to Euro with an implicit parameter
 *   of type [[org.scalalabs.basic.lab01.CurrencyConverter]]
 * - Use the implicit CurrencyConverter to do the conversion.
 *
 * Note:
 * For Exercise 4 and 5 you will need different versions of the conversion method.
 * It's okay if you can pass only either 4 or 5 at a time.
 */
abstract class Currency(val symbol: String)

class Euro(var euro: Int, var cents: Int = 0) extends Currency("EUR") with Ordered[Euro] {

  val inCents = euro * 100 + cents

  def +(euroToAdd: Euro): Euro = Euro.fromCents(inCents + euroToAdd.inCents)
  def *(multiplier: Int): Euro = Euro.fromCents(inCents * multiplier)
  def /(divider: Int): Euro = if (divider <= 0) throw new IllegalArgumentException else Euro.fromCents(inCents / divider)

  override def toString(): String = {
    val cent = if (cents == 0) "--" else if (cents < 10) "0" + cents else cents
    symbol + ": " + euro + "," + cent
  }

  override def compare(that: Euro): Int = inCents - that.inCents
  //if (this.inCents < that.inCents) -1 else if (this.inCents > that.inCents) 1 else 0

}

object Euro {
  def fromCents(initCents: Int): Euro = new Euro(initCents / 100, initCents % 100)

  implicit class IntImplicit(i: Int) {
    def *(euro: Euro) = euro * i
  }

  //Exercise 4
  //implicit def dollarToEuro(dollar: Dollar): Euro = Euro.fromCents(DefaultCurrencyConverter.toEuroCents(dollar.inCents))

  implicit def dollarToEuro(dollar: Dollar)(implicit converter: CurrencyConverter): Euro = Euro.fromCents(converter.toEuroCents(dollar.inCents))
}

class Dollar(var dollar: Int, var cents: Int = 0) {
  val inCents = dollar * 100 + cents
}
