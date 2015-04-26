

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
    val rivinLukija = new BufferedReader(tiedostoLukija)

    try {
      var rivi = rivinLukija.readLine()
      while (rivi != null) {
        var siistitty = rivi.replaceAll(" ", "")
        var katkaistu = siistitty.split(",")

        val nimi = katkaistu(0)
        val yksikko = katkaistu(1)
        val maara = katkaistu(2).toDouble
        val tiheys = katkaistu(3).toInt
        val allergeeni = if (katkaistu.size == 5) katkaistu(4) else ""
        varasto += new Aine(nimi, yksikko, maara, tiheys, allergeeni)
        rivi = rivinLukija.readLine()

      }
      varasto
    } catch {
      case e: IOException =>
        println("Lukuvirhe")
        return varasto
      case e: ArrayIndexOutOfBoundsException =>
        println("Virheellinen syöte.")
        return varasto
      case e: NumberFormatException =>
        println("Syötteessä numeron paikalla virheellistä tietoa")
        return varasto
    } finally {
      tiedostoLukija.close()
      rivinLukija.close()
    }
  }

  def tallennaVarasto(varasto: Buffer[Aine]): Boolean = {
    try {
      val kirjoittaja = new PrintWriter(new File("varasto.txt"))
      for (x <- varasto) {
        kirjoittaja.write(x.nimi + ", " + x.yksikko + ", " + x.maara + ", " + x.tiheys + ", " + x.allergeeni + "\n")
      }
      kirjoittaja.close()
      true
    } catch {
      case e: Exception =>
        println("Tallentaessa tapahtui virhe.")
        return false
    }
  }
}