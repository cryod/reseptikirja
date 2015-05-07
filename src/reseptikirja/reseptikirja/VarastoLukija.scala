

/**
 * @author Rasmus
 */
package reseptikirja

import java.io._
import scala.collection.mutable._

class VarastoLukija {

  // Aineen muodostusmetodi. K�ytet��n varaston lataamisessa sek� 
  // uusien aineiden lis��misess� muutenkin.
  def muodostaAine(rivi: String): Aine = {
    var katkaistu = rivi.split(",")
    val nimi = katkaistu(0).trim()
    val maara = katkaistu(1).toDouble
    val yksikko = katkaistu(2).trim()
    val tiheys = katkaistu(3).trim().toInt
    val allergeeni = if (katkaistu.size == 5) katkaistu(4).trim() else ""
    new Aine(nimi, maara, yksikko, tiheys, allergeeni)
  }

  // Ladataan varasto tiedostosta  muistiin. Ajetaan k�ynnistyksen yhteydess�.
  def lataaVarasto: Buffer[Aine] = {
    var varasto = Buffer.empty[Aine]
    val tiedostoLukija = try {
      new FileReader("varasto.txt")
    } catch {
      case e: FileNotFoundException =>
        println("Tiedostoa ei l�ydetty.")
        return varasto
    }
    val rivinLukija = new BufferedReader(tiedostoLukija)
    
    // Luetaan rivi kerrallaan ja lis�t��n aine varastoon.
    try {
      var rivi = rivinLukija.readLine()
      while (rivi != null) {
        if(!rivi.isEmpty())varasto += muodostaAine(rivi)
        rivi = rivinLukija.readLine()
      }
      varasto
    } catch {
      case e: IOException =>
        println("Lukuvirhe")
        return varasto
      case e: ArrayIndexOutOfBoundsException =>
        println("Virheellinen sy�te.")
        return varasto
      case e: NumberFormatException =>
        println(e.getMessage)
        println("Varaston sy�tteess� numeron paikalla virheellist� tietoa")
        return varasto
    } finally {
      tiedostoLukija.close()
      rivinLukija.close()
    }
  }

  // Tallennetaan varasto tiedostoon. Ajetaan sulkemisen yhteydess�, tai erikseen kutsuttuna.
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