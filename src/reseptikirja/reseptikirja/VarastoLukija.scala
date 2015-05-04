

/**
 * @author Rasmus
 */
package reseptikirja

import java.io._
import scala.collection.mutable._

class VarastoLukija {
  def muodostaAine(rivi: String): Aine ={
    var siistitty = rivi.replaceAll(" ", "").split(",")
    val nimi = siistitty(0)
    val maara = siistitty(1).toDouble
    val yksikko = siistitty(2)
    val tiheys = siistitty(3).toInt
    val allergeeni = if(siistitty.size == 5) siistitty(4) else ""
    new Aine(nimi, maara, yksikko, tiheys, allergeeni)
  }
  
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
        varasto += muodostaAine(rivi)
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
        kirjoittaja.write(x.nimi + ", " + x.maara + ", " + x.yksikko + ", " + x.tiheys + ", " + x.allergeeni + "\n")
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