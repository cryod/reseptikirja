package reseptikirja

import java.io._

object TextUI extends App {
  
  var exitFlag = false
  var mainMenu = true
  var reseptiMenu = false
  var reseptiMuokkaaMenu = false
  var reseptiEtsiMenu = false
  var varastoMenu = false
  
  println("Tervetuloa reseptikirjaan. \nValitse toiminto syˆtt‰m‰ll‰ toiminnon edess‰ oleva numero.")
  while(!exitFlag){
    seuraavaToiminto
  }
  
  
  
  def seuraavaToiminto {
    if(mainMenu){
      println
      println("1. Reseptit")
      println("2. Varasto")
      print("Valittu toiminto: ")
      val luettu = readLine()
      luettu match {
        case "1" => {
          mainMenu = false
          reseptiMenu = true
        }
        case "2" => {
          mainMenu = false
          varastoMenu = true
        }
        case _ =>
      }
    } else if(reseptiMenu){
      println
      println("1. Etsi reseptej‰")
      println("2. Lis‰‰ tai poista reseptej‰")
      println("0. Palaa p‰‰valikkoon")
      print("Valittu toiminto: ")
      val luettu = readLine()
      luettu match {
        case "1" => {
          reseptiMenu = false
          reseptiEtsiMenu = true
        }
        case "2" => {
          reseptiMenu = false
          reseptiMuokkaaMenu = true
        }
        case "0" => {
          reseptiMenu = false
          mainMenu = true
        }
        case _ =>
      }
    } else if(reseptiMuokkaaMenu) {
      println
      println("1. Lis‰‰ resepti")
      println("2. Poista resepti")
      println("0. Palaa p‰‰valikkoon")
      print("Valittu toiminto: ")
      val luettu = readLine()
      luettu match {
        case "1" => lisaaResepti
        case "2" => poistaResepti
        case "0" => {
          reseptiMenu = false
          mainMenu = true
        }
        case _ =>
      }
    }else if(reseptiEtsiMenu) {
      println
      println("1. Etsi nimen perusteella")
      println("2. Etsi kotona olevien aineiden perusteella")
      println("3. Etsi ainesosan perusteella")
      println("4. Etsi ")
      
      
    } else if(varastoMenu){
      println
      println("1. Tarkista varastotilanne")
      println("2. Lis‰‰ aine varastoon")
      println("3. V‰henn‰ varastosta")
      println("0. Palaa p‰‰valikkoon")
      print("Valittu toiminto: ")
      val luettu = readLine()
      luettu match {
        case "1" => tulostaVarasto
        case "2" => lisaaVarastoon
        case "3" => poistaVarastosta
        case "0" => {
          varastoMenu = false
          mainMenu = true
        }
        case _ =>
      }
    }
  }
  
  def lisaaResepti {
    println("lis‰‰Resepti placeholder")
  }
  def poistaResepti {
    println("poistaResepti placeholder")
  }
  def tulostaVarasto {
    println("tulostaVarasto placeholder")
  }
  def lisaaVarastoon {
    println("lisaaVarastoon placeholder")
  }
  def poistaVarastosta {
    println("poistaVarastosta placeholder")
  }
  
  

}