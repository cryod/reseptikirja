

/**
 * @author Rasmus
 */
package reseptikirja

import java.io._
import scala.collection.mutable._

class VarastoLukija {
  def lataaVarasto: Buffer[Aine] = {
    var varasto = Buffer.empty[Aine]
    val tiedostoLukija = try {
      new FileReader("varasto.txt")
    } catch {
      case e: FileNotFoundException =>
        println("Tiedostoa ei löydetty.")
        return varasto
    }
    val riviLukija = new BufferedReader(tiedostoLukija)

    try {
      var rivi = riviLukija.readLine()
      while (rivi != null) {
        var katkaistu = rivi.split(",")
        katkaistu.foreach { _.trim() }
        val nimi = katkaistu(0)
        val yksikko = katkaistu(1)
        val maara = katkaistu(2).toDouble
        val tiheys = katkaistu(3).toInt
        val allergeeni = if (katkaistu.size == 5) katkaistu(4) else ""
        varasto += new Aine(nimi, yksikko, maara, tiheys, allergeeni)
        rivi = riviLukija.readLine()

      }
      varasto
    } catch {
      case e: IOException =>
        println("Lukuvirhe")
        return varasto

    } finally {
      tiedostoLukija.close()
      riviLukija.close()

    }

  }
}