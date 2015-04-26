

package reseptikirja/**
 * @author Rasmus
 */
class Resepti(val nimi: String, val aineet:Vector[(String,Double,String)], val ohje: String) {
  
  override def toString = {
    var tuloste = nimi + "\n"
    for (x <- aineet) {
      tuloste += x._1 +", " + x._2 +" " + x._3 + "\n"
    }
    tuloste += ohje + "\n"
    tuloste
  }
}