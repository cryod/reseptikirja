

package reseptikirja
/**
 * @author Rasmus
 */
class Aine(val nimi: String, val yksikko: String, var maara: Double, val tiheys: Int, val allergeeni: String) {
  override def toString = {
    nimi+", " +yksikko+", "+ maara+ ", " + tiheys +", " +allergeeni
  }
}