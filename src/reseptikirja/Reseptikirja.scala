
package reseptikirja


import scala.collection.mutable._
/**
 * @author Rasmus
 */
class Reseptikirja {
  

  val varastonLukija = new VarastoLukija
  
  val a = varastonLukija.lataaVarasto
  a.foreach { println(_) }
  
}