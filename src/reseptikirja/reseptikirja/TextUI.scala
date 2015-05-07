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
  println("Tervetuloa reseptikirjaan. \nValitse toiminto syöttämällä toiminnon edessä oleva numero.")
  // Haetaan seuraava toiminto niin pitkään kun ei sammuteta
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
  // Suljettaessa tehdään nämä
  println("Varasto ja reseptit tallennetaan sekä ohjelma suljetaan.")
  reseptikirja.tallennaReseptit
  reseptikirja.tallennaVarasto

  // Valikkojen käytännön totetutus
  // Tarkistetaan mikä flag on päällä, jotta tiedetään mitä tehdä.
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
      println("1. Etsi reseptejä")
      println("2. Lisää tai poista reseptejä")
      println("0. Palaa päävalikkoon")
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
      println("1. Lisää resepti")
      println("2. Poista resepti")
      println("0. Palaa päävalikkoon")
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
      println("0. Palaa päävalikkoon")
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
      println("2. Lisää aine varastoon")
      println("3. Vähennä varastosta")
      println("4. Poista varastosta")
      println("0. Palaa päävalikkoon")
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
  
  // Reseptin lisäys 
  def lisaaResepti {
    // try-catch lohkot, koska otetaan tekstiä suoraan komentoriviltä.
    try {
      print("Syötä reseptin nimi: ")
      val nimi = readLine()
      var aineet = Buffer.empty[(String, Double, String)]
      println("Kun kaikki aineet on syötetty, syötä tyhjät rivit.")
      var flag = false
      // Kierretään niin pitkään läpi kun on syötettäviä aineita
      while (!flag) {
        print("Syötä aineen nimi: ")
        val nimiA = readLine()
        print("Syötä määrä: ")
        val maara = readLine()
        print("Syötä yksikkö: ")
        val yksikko = readLine()
        if (!nimiA.isEmpty()) aineet += ((nimiA, maara.toDouble, yksikko))
        if (nimiA.isEmpty()) flag = true
      }
      print("Syötä ohje: ")
      val ohje = readLine()
      reseptikirja.lisaaResepti(nimi, aineet.toVector, ohje)

    } catch {
      case _: Throwable => println("Virheellinen syöte.")
    }
  }
  
  // Reseptin poisto. Pyydetään nimi, katsotaan löytyykö ja poistetaan jos mahdollsita.
  def poistaResepti {
    print("Syötä poistettavan reseptin nimi: ")
    val nimi = readLine()
    if (reseptikirja.reseptit.contains(nimi)) {
      reseptikirja.poistaResepti(nimi)
      println("Resepti poistettu")
    } else println("Reseptiä ei löydetty.")
  }

  // Tulostetaan varaston sisältö aine kerrallaan.
  def tulostaVarasto {
    println
    println("Varastosta löytyvät aineet: ")
    println("nimi|määrä|yksikkö|tiheys(g/l)|mahdollinen allergeeni")
    reseptikirja.varasto.values.foreach { println(_) }
  }
  
  // Lisätään varastoon aine(tta). Jos ainetta on jo, nimi, määrä, yksikkö riittää, muuten kysytään tiheys ja allergeeni.
  def lisaaVarastoon {
    try {
      print("Syötä aineen nimi: ")
      val nimi = readLine()
      print("Syötä määrä: ")
      val maara = readLine().toDouble
      print("Syötä yksikkö: ")
      val yksikko = readLine()
      if (reseptikirja.varasto.contains(nimi)) {
        val vanhaAine = reseptikirja.varasto(nimi)
        val rivi = nimi + ", " + maara + ", " + yksikko + ", " + vanhaAine.tiheys + ", " + vanhaAine.allergeeni
        reseptikirja.lisaaVarastoon(rivi)
        println("Aine lisätty varastoon!")
      } else {
        print("Syötä tiheys(g/l): ")
        val tiheys = readLine().toInt
        print("Syötä mahdollinen allergeeni: ")
        val allergeeni = readLine()
        val rivi = nimi + ", " + maara + ", " + yksikko + ", " + tiheys + ", " + allergeeni
        reseptikirja.lisaaVarastoon(rivi)
        println("Aine lisätty varastoon!")
      }
    } catch {
      case _: Throwable => println("Virhe syötteessä.")
    }
  }
  
  // Vähennetään ainetta varastosta. Kysytään nimi, määrä, yksikkö. Jos ainetta ei löydy, ei tehdä mitään.
  def vahennaVarastosta {
    try {
      print("Syötä vähennettävän aineen nimi: ")
      val nimi = readLine()
      print("Syötä vähennettävä määrä: ")
      val maara = readLine().toDouble
      print("Syötä yksikkö: ")
      val yksikko = readLine()
      if (reseptikirja.varasto.contains(nimi)) {
        val rivi = nimi + ", " + maara + ", " + yksikko
        reseptikirja.vahennaVarastosta(rivi)
        println("Ainetta vähennetty varastosta!")
      } else println("Ainetta ei löytynyt varastosta!")
    } catch {
      case _: Throwable => println("Virhe syötteessä.")
    }
  }

  // Poistetaan aine kokonaan varastosta. Jos ei löydy, ei tehdä mitään.
  def poistaVarastosta {
    print("Syötä poistettavan aineen nimi: ")
    val nimi = readLine()
    if (reseptikirja.varasto.contains(nimi)) {
      reseptikirja.poistaAine(nimi)
      println("Aine poistettu.")
    } else println("Ainetta ei löydetty.")
  }
  
  // Etsitään resepti nimellä. Jos ei löydy yhtään, todetaan niin. Muuten valitaan haluttu resepti erillisellä metodilla.
  def etsiNimella {
    val allergeeni = kysyAllergeeni
    println()
    print("Syötä reseptin nimi: ")
    val luettu = readLine()
    val reseptit = reseptikirja.etsija.nimi(luettu, reseptikirja.reseptit, reseptikirja.varasto, allergeeni)
    if (reseptit.isEmpty) println("Yhtään reseptiä ei löytynyt.")
    else valitseResepti(reseptit)
  }
  
  // Etsitään varastosta löytyvien aineiden perusteella. Kysytään puuttuvien aineiden maksimimäärä. Lopulta valitaan erillisellä metodilla.
  def etsiKotona {
    try {
      val allergeeni = kysyAllergeeni
      println()
      print("Syötä puuttuvien aineiden maksimimäärä: ")
      var maara = readLine()
      if (maara.isEmpty()) maara = "0"
      val reseptit = reseptikirja.etsija.kotona(maara.toInt, reseptikirja.reseptit, reseptikirja.varasto, allergeeni, reseptikirja.muunnos)
      if (reseptit.isEmpty) println("Yhtään reseptiä ei löytynyt.")
      else valitseResepti(reseptit)
    } catch {
      case _: Throwable => println("Virhe syötteessä")
    }
  }

  // Etsitään resepti tietyn raaka-aineen perusteella. Taas valitaan lopulta.
  def etsiAine {
    try {
      val allergeeni = kysyAllergeeni
      println()
      print("Syötä ainesosa jota etsitään: ")
      val aine = readLine()
      val reseptit = reseptikirja.etsija.ainesOsa(aine, reseptikirja.reseptit, reseptikirja.varasto, allergeeni)
      if (reseptit.isEmpty) println("Yhtään reseptiä ei löytynyt.")
      else valitseResepti(reseptit)
    } catch {
      case _: Throwable => println("Virhe syötteessä")
    }
  }

  // Tulostaa kaikki löydettyjen reseptien nimet. Tulostaa sen jälkeen valitun reseptin kokonaan.
  def valitseResepti(reseptit: Buffer[Resepti]) {
    println()
    println("Löydetyt reseptit:")
    reseptit.foreach { x => println(x.nimi) }
    println
    print("Syötä halutun reseptin nimi:")
    val luettu = readLine()
    for (x <- reseptit) {
      if (x.nimi == luettu) {
        println(x)
        return
      }
    }
    println("Syöte ei vastannut yhdenkään reseptin nimeä.")
  }

  // Kysyy mahdollisen poisjätettävän allergeenin ja palauttaa sen.
  def kysyAllergeeni: String = {
    try {
      println()
      print("Syötä poisjätettävä allergeeni tai tyhjä rivi: ")
      val allergeeni = readLine()
      allergeeni
    } catch {
      case _: Throwable => {
        println("Virhe syötteessä, allergeeniä ei lisätty hakuun.")
        ""
      }
    }
  }

}