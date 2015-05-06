

package reseptikirja
/**
 * @author Rasmus
 */

import java.io._
import scala.collection.mutable._
class ReseptiLukija {
  
  // Lataa reseptit tiedostosta. Ajetaan k�ynnistyksen yhteydess�.
  def lataaReseptit: Buffer[Resepti] = {
    var reseptit = Buffer.empty[Resepti]
    val tiedostonLukija = try {
      new FileReader("reseptit.txt")
    } catch {
      case e: FileNotFoundException =>
        println("Tiedostoa ei l�ydetty.")
        return reseptit
    }
    val rivinLukija = new BufferedReader(tiedostonLukija)
    
    // Lukee reseptin kerrallaan, kunnes tiedosto p��ttyy. Reseptien v�liss� voi olla tyhji� rivej�
    // mutta yhden reseptin pit�� olla aina per�kk�isill� riveill�.
    // Reseptin alku tunnistetaan # merkill�, jonka j�lkeen haetaan aineet.
    try {
      var rivi = rivinLukija.readLine()
      while (rivi != null) {
        if (rivi.contains("#")) {
          val nimi = rivi.tail
          rivi = rivinLukija.readLine()
          val aineMaara = rivi.toInt
          var aineet = Buffer.empty[(String, Double, String)]
          // K�yd��n aineet l�pi yksi kerrallaan. Siistit��n turhat v�lit pois.
          // Tallennetaan bufferiin, joka lopulta muutetaan vectoriksi kun muutetaan reseptiksi.
          for (x <- 0 until aineMaara) {
            rivi = rivinLukija.readLine()
            var katkaistu = rivi.split(",")
            var nimi = katkaistu(0).trim()
            var yksikko = katkaistu(2).trim()
            aineet += ((nimi, katkaistu(1).toDouble, yksikko))
          }
          rivi = rivinLukija.readLine()
          reseptit += new Resepti(nimi, aineet.toVector, rivi)
          rivi = rivinLukija.readLine()
        } else rivi = rivinLukija.readLine()
      }
      reseptit

    } catch {
      case e: IOException =>
        println("Lukuvirhe")
        return reseptit
      case e: ArrayIndexOutOfBoundsException =>
        println("Virheellinen sy�te.")
        return reseptit
      case e: NumberFormatException =>
        println("Sy�tteess� numeron paikalla virheellist� tietoa")
        return reseptit

    } finally {
      tiedostonLukija.close()
      rivinLukija.close()
    }
  }

  
  // Tallentaa reseptit tiedostoon. Ajetaan ohjelman sulkemisen yhteydess�, tai erikseen kutsuttuna.
  def tallennaReseptit(reseptit: Buffer[Resepti]): Boolean = {
    try {
      val kirjoittaja = new PrintWriter(new File("reseptit.txt"))
      for (x <- reseptit) {
        kirjoittaja.write("#" + x.nimi + "\n")
        kirjoittaja.write(x.aineet.size + "\n")
        for (y <- x.aineet) {
          kirjoittaja.write(y._1 + ", " + y._2 + ", " + y._3 + "\n")
        }
        kirjoittaja.write(x.ohje + "\n")
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