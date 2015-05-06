

package reseptikirja
/**
 * @author Rasmus
 */

import java.io._
import scala.collection.mutable._
class ReseptiLukija {
  
  // Lataa reseptit tiedostosta. Ajetaan käynnistyksen yhteydessä.
  def lataaReseptit: Buffer[Resepti] = {
    var reseptit = Buffer.empty[Resepti]
    val tiedostonLukija = try {
      new FileReader("reseptit.txt")
    } catch {
      case e: FileNotFoundException =>
        println("Tiedostoa ei löydetty.")
        return reseptit
    }
    val rivinLukija = new BufferedReader(tiedostonLukija)
    
    // Lukee reseptin kerrallaan, kunnes tiedosto päättyy. Reseptien välissä voi olla tyhjiä rivejä
    // mutta yhden reseptin pitää olla aina peräkkäisillä riveillä.
    // Reseptin alku tunnistetaan # merkillä, jonka jälkeen haetaan aineet.
    try {
      var rivi = rivinLukija.readLine()
      while (rivi != null) {
        if (rivi.contains("#")) {
          val nimi = rivi.tail
          rivi = rivinLukija.readLine()
          val aineMaara = rivi.toInt
          var aineet = Buffer.empty[(String, Double, String)]
          // Käydään aineet läpi yksi kerrallaan. Siistitään turhat välit pois.
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
        println("Virheellinen syöte.")
        return reseptit
      case e: NumberFormatException =>
        println("Syötteessä numeron paikalla virheellistä tietoa")
        return reseptit

    } finally {
      tiedostonLukija.close()
      rivinLukija.close()
    }
  }

  
  // Tallentaa reseptit tiedostoon. Ajetaan ohjelman sulkemisen yhteydessä, tai erikseen kutsuttuna.
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