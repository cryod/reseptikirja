

package reseptikirja
/**
 * @author Rasmus
 */

// 
class Aine(val nimi: String,  var maara: Double, val yksikko: String, val tiheys: Int, val allergeeni: String) {
  override def toString = {
    nimi+", " +maara+", "+ yksikko+ ", " + tiheys +", " +allergeeni
  }
}