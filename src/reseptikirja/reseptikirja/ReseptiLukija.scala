

package reseptikirja
/**
 * @author Rasmus
 */

import java.io._
import scala.collection.mutable._
class ReseptiLukija {
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

    try {
      var rivi = rivinLukija.readLine()
      while (rivi != null) {
        if (rivi.contains("#")) {
          val nimi = rivi.tail
          rivi = rivinLukija.readLine()
          val aineMaara = rivi.toInt
          var aineet = Buffer.empty[(String, Double, String)]
          for (x <- 0 until aineMaara) {
            rivi = rivinLukija.readLine()
            var siistitty = rivi.replaceAll(" ", "")
            var katkaistu = siistitty.split(",")
            aineet += ((katkaistu(0), katkaistu(1).toDouble, katkaistu(2)))
          }
          rivi = rivinLukija.readLine()
          reseptit += new Resepti(nimi, aineet.toVector, rivi)
          rivi = rivinLukija.readLine()
        }
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