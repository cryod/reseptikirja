package reseptikirja

import java.io._
import scala.collection.mutable._

object TextUI extends App {
  
  
  // Valikkologiikkaa
  var exitFlag = false
  var mainMenu = true
  var reseptiMenu = false
  var reseptiMuokkaaMenu = false
  var reseptiEtsiMenu = false
  var varastoMenu = false
  // Avataan kirja
  val reseptikirja = new Reseptikirja
  println("Tervetuloa reseptikirjaan. \nValitse toiminto sy�tt�m�ll� toiminnon edess� oleva numero.")
  // Haetaan seuraava toiminto niin pitk��n kun ei sammuteta
  while (!exitFlag) {
    seuraavaToiminto
    if (mainMenu) {
      var reseptiMenu = false
      var reseptiMuokkaaMenu = false
      var reseptiEtsiMenu = false
      var varastoMenu = false
    }
    if (!mainMenu && !reseptiMenu && !reseptiMuokkaaMenu
      && !reseptiEtsiMenu && !varastoMenu) mainMenu = true
    println("------------------------------------------------------")
  }
  // Suljettaessa tehd��n n�m�
  println("Varasto ja reseptit tallennetaan sek� ohjelma suljetaan.")
  reseptikirja.tallennaReseptit
  reseptikirja.tallennaVarasto

  // Valikkojen k�yt�nn�n totetutus
  // Tarkistetaan mik� flag on p��ll�, jotta tiedet��n mit� tehd�.
  // *** erottaa if-else lauseet, --- erottaa caset toisistaan
  def seuraavaToiminto {
//*********************************************
    if (mainMenu) {
      println
      println("1. Reseptit")
      println("2. Varasto")
      println("0. Sammuta ohjelma")
      print("Valittu toiminto: ")
      val luettu = readLine()
      luettu match {
//----------------------------------------
        case "1" => {
          mainMenu = false
          reseptiMenu = true
        }
//----------------------------------------        
        case "2" => {
          mainMenu = false
          varastoMenu = true
        }
//----------------------------------------        
        case "0" => exitFlag = true
//----------------------------------------        
        case _ =>   
      }
//*********************************************      
    } else if (reseptiMenu) {
      println
      println("1. Etsi reseptej�")
      println("2. Lis�� tai poista reseptej�")
      println("0. Palaa p��valikkoon")
      print("Valittu toiminto: ")
      val luettu = readLine()
      luettu match {
//----------------------------------------        
        case "1" => {
          reseptiMenu = false
          reseptiEtsiMenu = true
        }
//----------------------------------------        
        case "2" => {
          reseptiMenu = false
          reseptiMuokkaaMenu = true
        }
//----------------------------------------        
        case "0" => {
          reseptiMenu = false
          mainMenu = true
        }
//----------------------------------------        
        case _ =>
      }
//*********************************************      
    } else if (reseptiMuokkaaMenu) {
      println
      println("1. Lis�� resepti")
      println("2. Poista resepti")
      println("0. Palaa p��valikkoon")
      print("Valittu toiminto: ")
      val luettu = readLine()
      luettu match {
//----------------------------------------        
        case "1" => {
          lisaaResepti
          reseptiMuokkaaMenu = false
          mainMenu = true
        }
//----------------------------------------        
        case "2" => {
          poistaResepti
          reseptiMuokkaaMenu = false
          mainMenu = true
        }
//----------------------------------------        
        case "0" => {
          reseptiMuokkaaMenu = false
          mainMenu = true
        }
//----------------------------------------        
        case _ =>
      }
//*********************************************      
    } else if (reseptiEtsiMenu) {
      println
      println("1. Etsi nimen perusteella")
      println("2. Etsi kotona olevien aineiden perusteella")
      println("3. Etsi ainesosan perusteella")
      println("0. Palaa p��valikkoon")
      print("Valittu toiminto: ")
      val luettu = readLine()
      luettu match {
//----------------------------------------        
        case "1" => {
          etsiNimella
          reseptiEtsiMenu = false
          mainMenu = true
        }
//----------------------------------------        
        case "2" => {
          etsiKotona
          reseptiEtsiMenu = false
          mainMenu = true
        }
//----------------------------------------        
        case "3" => {
          etsiAine
          reseptiEtsiMenu = false
          mainMenu = true
        }
//----------------------------------------        
        case "0" => {
          reseptiEtsiMenu = false
          mainMenu = true
        }
//----------------------------------------        
        case _ =>
      }
//*********************************************
    } else if (varastoMenu) {
      println
      println("1. Tarkista varastotilanne")
      println("2. Lis�� aine varastoon")
      println("3. V�henn� varastosta")
      println("4. Poista varastosta")
      println("0. Palaa p��valikkoon")
      print("Valittu toiminto: ")
      val luettu = readLine()
      luettu match {
        case "1" => tulostaVarasto
        case "2" => lisaaVarastoon
        case "3" => vahennaVarastosta
        case "4" => poistaVarastosta
        case "0" => {
          varastoMenu = false
          mainMenu = true
        }
        case _ =>
      }
    }
  }
//*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
  
  // Reseptin lis�ys 
  def lisaaResepti {
    // try-catch lohkot, koska otetaan teksti� suoraan komentorivilt�.
    try {
      print("Sy�t� reseptin nimi: ")
      val nimi = readLine()
      var aineet = Buffer.empty[(String, Double, String)]
      println("Kun kaikki aineet on sy�tetty, sy�t� tyhj�t rivit.")
      var flag = false
      // Kierret��n niin pitk��n l�pi kun on sy�tett�vi� aineita
      while (!flag) {
        print("Sy�t� aineen nimi: ")
        val nimiA = readLine()
        print("Sy�t� m��r�: ")
        val maara = readLine()
        print("Sy�t� yksikk�: ")
        val yksikko = readLine()
        if (!nimiA.isEmpty()) aineet += ((nimiA, maara.toDouble, yksikko))
        if (nimiA.isEmpty()) flag = true
      }
      print("Sy�t� ohje: ")
      val ohje = readLine()
      reseptikirja.lisaaResepti(nimi, aineet.toVector, ohje)

    } catch {
      case _: Throwable => println("Virheellinen sy�te.")
    }
  }
  
  // Reseptin poisto. Pyydet��n nimi, katsotaan l�ytyyk� ja poistetaan jos mahdollsita.
  def poistaResepti {
    print("Sy�t� poistettavan reseptin nimi: ")
    val nimi = readLine()
    if (reseptikirja.reseptit.contains(nimi)) {
      reseptikirja.poistaResepti(nimi)
      println("Resepti poistettu")
    } else println("Resepti� ei l�ydetty.")
  }

  // Tulostetaan varaston sis�lt� aine kerrallaan.
  def tulostaVarasto {
    println
    println("Varastosta l�ytyv�t aineet: ")
    println("nimi|m��r�|yksikk�|tiheys(g/l)|mahdollinen allergeeni")
    reseptikirja.varasto.values.foreach { println(_) }
  }
  
  // Lis�t��n varastoon aine(tta). Jos ainetta on jo, nimi, m��r�, yksikk� riitt��, muuten kysyt��n tiheys ja allergeeni.
  def lisaaVarastoon {
    try {
      print("Sy�t� aineen nimi: ")
      val nimi = readLine()
      print("Sy�t� m��r�: ")
      val maara = readLine().toDouble
      print("Sy�t� yksikk�: ")
      val yksikko = readLine()
      if (reseptikirja.varasto.contains(nimi)) {
        val vanhaAine = reseptikirja.varasto(nimi)
        val rivi = nimi + ", " + maara + ", " + yksikko + ", " + vanhaAine.tiheys + ", " + vanhaAine.allergeeni
        reseptikirja.lisaaVarastoon(rivi)
        println("Aine lis�tty varastoon!")
      } else {
        print("Sy�t� tiheys(g/l): ")
        val tiheys = readLine().toInt
        print("Sy�t� mahdollinen allergeeni: ")
        val allergeeni = readLine()
        val rivi = nimi + ", " + maara + ", " + yksikko + ", " + tiheys + ", " + allergeeni
        reseptikirja.lisaaVarastoon(rivi)
        println("Aine lis�tty varastoon!")
      }
    } catch {
      case _: Throwable => println("Virhe sy�tteess�.")
    }
  }
  
  // V�hennet��n ainetta varastosta. Kysyt��n nimi, m��r�, yksikk�. Jos ainetta ei l�ydy, ei tehd� mit��n.
  def vahennaVarastosta {
    try {
      print("Sy�t� v�hennett�v�n aineen nimi: ")
      val nimi = readLine()
      print("Sy�t� v�hennett�v� m��r�: ")
      val maara = readLine().toDouble
      print("Sy�t� yksikk�: ")
      val yksikko = readLine()
      if (reseptikirja.varasto.contains(nimi)) {
        val rivi = nimi + ", " + maara + ", " + yksikko
        reseptikirja.vahennaVarastosta(rivi)
        println("Ainetta v�hennetty varastosta!")
      } else println("Ainetta ei l�ytynyt varastosta!")
    } catch {
      case _: Throwable => println("Virhe sy�tteess�.")
    }
  }

  // Poistetaan aine kokonaan varastosta. Jos ei l�ydy, ei tehd� mit��n.
  def poistaVarastosta {
    print("Sy�t� poistettavan aineen nimi: ")
    val nimi = readLine()
    if (reseptikirja.varasto.contains(nimi)) {
      reseptikirja.poistaAine(nimi)
      println("Aine poistettu.")
    } else println("Ainetta ei l�ydetty.")
  }
  
  // Etsit��n resepti nimell�. Jos ei l�ydy yht��n, todetaan niin. Muuten valitaan haluttu resepti erillisell� metodilla.
  def etsiNimella {
    val allergeeni = kysyAllergeeni
    println()
    print("Sy�t� reseptin nimi: ")
    val luettu = readLine()
    val reseptit = reseptikirja.etsija.nimi(luettu, reseptikirja.reseptit, reseptikirja.varasto, allergeeni)
    if (reseptit.isEmpty) println("Yht��n resepti� ei l�ytynyt.")
    else valitseResepti(reseptit)
  }
  
  // Etsit��n varastosta l�ytyvien aineiden perusteella. Kysyt��n puuttuvien aineiden maksimim��r�. Lopulta valitaan erillisell� metodilla.
  def etsiKotona {
    try {
      val allergeeni = kysyAllergeeni
      println()
      print("Sy�t� puuttuvien aineiden maksimim��r�: ")
      var maara = readLine()
      if (maara.isEmpty()) maara = "0"
      val reseptit = reseptikirja.etsija.kotona(maara.toInt, reseptikirja.reseptit, reseptikirja.varasto, allergeeni, reseptikirja.muunnos)
      if (reseptit.isEmpty) println("Yht��n resepti� ei l�ytynyt.")
      else valitseResepti(reseptit)
    } catch {
      case _: Throwable => println("Virhe sy�tteess�")
    }
  }

  // Etsit��n resepti tietyn raaka-aineen perusteella. Taas valitaan lopulta.
  def etsiAine {
    try {
      val allergeeni = kysyAllergeeni
      println()
      print("Sy�t� ainesosa jota etsit��n: ")
      val aine = readLine()
      val reseptit = reseptikirja.etsija.ainesOsa(aine, reseptikirja.reseptit, reseptikirja.varasto, allergeeni)
      if (reseptit.isEmpty) println("Yht��n resepti� ei l�ytynyt.")
      else valitseResepti(reseptit)
    } catch {
      case _: Throwable => println("Virhe sy�tteess�")
    }
  }

  // Tulostaa kaikki l�ydettyjen reseptien nimet. Tulostaa sen j�lkeen valitun reseptin kokonaan.
  def valitseResepti(reseptit: Buffer[Resepti]) {
    println()
    println("L�ydetyt reseptit:")
    reseptit.foreach { x => println(x.nimi) }
    println
    print("Sy�t� halutun reseptin nimi:")
    val luettu = readLine()
    for (x <- reseptit) {
      if (x.nimi == luettu) {
        println(x)
        return
      }
    }
    println("Sy�te ei vastannut yhdenk��n reseptin nime�.")
  }

  // Kysyy mahdollisen poisj�tett�v�n allergeenin ja palauttaa sen.
  def kysyAllergeeni: String = {
    try {
      println()
      print("Sy�t� poisj�tett�v� allergeeni tai tyhj� rivi: ")
      val allergeeni = readLine()
      allergeeni
    } catch {
      case _: Throwable => {
        println("Virhe sy�tteess�, allergeeni� ei lis�tty hakuun.")
        ""
      }
    }
  }

}