package reseptikirja

import scala.swing._
import scala.swing.event._
import scala.swing.GridBagPanel.Anchor._
import scala.swing.GridBagPanel.Fill
import javax.swing.UIManager
import scala.swing.BorderPanel.Position._
import event._
import java.awt.{ Color, Graphics2D }
import javax.swing.SpringLayout.Constraints
import javax.swing.JPanel
import java.awt.GridLayout
import com.sun.corba.se.impl.activation.ListORBs
import Key._
import scala.collection.mutable._

object GUI extends SimpleSwingApplication {

  val frame = new MainFrame {

    title = "Reseptikirja"
    minimumSize = new Dimension(550, 700)
    preferredSize = new Dimension(600, 700)
    resizable = true
    visible = true
    peer.setLocationRelativeTo(null)

    val reseptikirja = new Reseptikirja

    var teksti = ""
    var allergeeniT = ""
    var nimiT = ""
    var aineNimiT = ""
    var maaraT = ""
    var yksikkoT = ""
    var tiheysT = ""
    var ohjeT = ""
    var aineetT = Buffer.empty[(String, Double, String)]
    var tila = "main"
    var reseptitT = Buffer.empty[Resepti]

    this.menuBar = new MenuBar {
      contents += new Menu("Ohjelma") {
        val quitAction = Action("Sammuta") {
          reseptikirja.tallennaReseptit
          reseptikirja.tallennaVarasto
          dispose()
        }
        contents += new MenuItem(quitAction)
      }
    }

    val contentHolder = new BorderPanel()

    val output = new TextArea(35, 50) {
      editable = false
      wordWrap = true
      lineWrap = true
      text = "Tervetuloa k�ytt�m��n reseptikirjaa.\nValitse haluamasi toiminto vasemmalta laidalta, ja seuraa ohjeita n�yt�ll�."
    }

    val input = new TextField(20)

    this.listenTo(input.keys)

    val etsiNimiNappi = new Button
    etsiNimiNappi.background = java.awt.Color.lightGray
    etsiNimiNappi.text = "Etsi nimen perusteella"
    listenTo(etsiNimiNappi)

    val etsiKotonaNappi = new Button
    etsiKotonaNappi.background = java.awt.Color.lightGray
    etsiKotonaNappi.text = "Etsi kotona olevista aineista"
    listenTo(etsiKotonaNappi)

    val etsiAinesNappi = new Button
    etsiAinesNappi.background = java.awt.Color.lightGray
    etsiAinesNappi.text = "Etsi ainesosan perusteella"
    listenTo(etsiAinesNappi)

    val lisaaReseptiNappi = new Button
    lisaaReseptiNappi.background = java.awt.Color.lightGray
    lisaaReseptiNappi.text = "Lis�� resepti"
    listenTo(lisaaReseptiNappi)

    val poistaReseptiNappi = new Button
    poistaReseptiNappi.background = java.awt.Color.lightGray
    poistaReseptiNappi.text = "Poista resepti"
    listenTo(poistaReseptiNappi)

    val tarkistaVarastoNappi = new Button
    tarkistaVarastoNappi.background = java.awt.Color.lightGray
    tarkistaVarastoNappi.text = "Tarkista varastotilanne"
    listenTo(tarkistaVarastoNappi)

    val lisaaVarastoonNappi = new Button
    lisaaVarastoonNappi.background = java.awt.Color.lightGray
    lisaaVarastoonNappi.text = "Lis�� varastoon"
    listenTo(lisaaVarastoonNappi)

    val vahennaVarastostaNappi = new Button
    vahennaVarastostaNappi.background = java.awt.Color.lightGray
    vahennaVarastostaNappi.text = "V�henn� varastosta"
    listenTo(vahennaVarastostaNappi)

    val poistaVarastostaNappi = new Button
    poistaVarastostaNappi.background = java.awt.Color.lightGray
    poistaVarastostaNappi.text = "Poista varastosta"
    listenTo(poistaVarastostaNappi)

    val napit = new BoxPanel(swing.Orientation.Vertical)
    napit.visible = true

    napit.minimumSize = new Dimension(200, 700)
    napit.contents += new Label(" ")
    napit.contents += new Label("             Etsi reseptej�")
    napit.contents += etsiNimiNappi
    napit.contents += etsiKotonaNappi
    napit.contents += etsiAinesNappi
    napit.contents += new Label(" ")
    napit.contents += new Label("           Hallinnoi reseptej�")
    napit.contents += lisaaReseptiNappi
    napit.contents += poistaReseptiNappi
    napit.contents += new Label(" ")
    napit.contents += new Label("            Hallinnoi varastoa")
    napit.contents += tarkistaVarastoNappi
    napit.contents += lisaaVarastoonNappi
    napit.contents += vahennaVarastostaNappi
    napit.contents += poistaVarastostaNappi

    contentHolder.layout(napit) = BorderPanel.Position.West
    contentHolder.layout(output) = BorderPanel.Position.Center
    contentHolder.layout(input) = BorderPanel.Position.South

    contents = contentHolder
    reactions += {
      case click: ButtonClicked =>
        val lahde = click.source
        nollaaTila()
        lahde.text match {
          case "Etsi nimen perusteella" => etsiNimella
          case "Etsi kotona olevista aineista" => etsiKotona
          case "Etsi ainesosan perusteella" => etsiAine
          case "Lis�� resepti" => lisaaResepti
          case "Poista resepti" => poistaResepti
          case "Tarkista varastotilanne" => tarkistaVarasto
          case "Lis�� varastoon" => lisaaVarastoon
          case "V�henn� varastosta" => vahennaVarastosta
          case "Poista varastosta" => poistaVarastosta
          case _ =>
        }
      case KeyPressed(_, Key.Enter, _, _) => {
        teksti = input.text
        input.text = ""
        if (tila.contains("Nimella")) etsiNimella
        else if (tila.contains("valitse")) valitseResepti(reseptitT)
        else if (tila.contains("etsiAine")) etsiAine
        else if (tila.contains("etsiKotona")) etsiKotona
        else if (tila.contains("lisaaResepti")) lisaaResepti
        else if (tila.contains("poistaResepti")) poistaResepti
        else if (tila.contains("tarkistaVarasto")) tarkistaVarasto
        else if (tila.contains("lisaaVarastoon")) lisaaVarastoon
        else if (tila.contains("vahennaVarastosta")) vahennaVarastosta
        else if (tila.contains("poistaVarastosta")) poistaVarastosta

      }
    }

    def etsiNimella {
      tila match {
        case "main" => {
          output.text = "Etsi reseptin nimell�."
          kysyAllergeeni
          tila = "etsiNimella1"
        }
        case "etsiNimella1" => {
          allergeeniT = teksti
          output.append("\nAllergeeni: " + allergeeniT + "\nSy�t� etsitt�v� nimi.")
          tila = "etsiNimella2"
        }
        case "etsiNimella2" => {
          nimiT = teksti
          val reseptit = reseptikirja.etsija.nimi(nimiT, reseptikirja.reseptit, reseptikirja.varasto, allergeeniT)
          tila = "valitse"
          valitseResepti(reseptit)
        }
      }
    }

    def etsiAine {
      tila match {
        case "main" => {
          output.text = "Etsi k�ytett�v�ll� ainesosalla."
          kysyAllergeeni
          tila = "etsiAine1"
        }
        case "etsiAine1" => {
          allergeeniT = teksti
          output.append("\nAllergeeni: " + allergeeniT + "\nSy�t� etsitt�v�n ainesosan nimi.")
          tila = "etsiAine2"
        }
        case "etsiAine2" => {
          nimiT = teksti
          val reseptit = reseptikirja.etsija.ainesOsa(nimiT, reseptikirja.reseptit, reseptikirja.varasto, allergeeniT)
          tila = "valitse"
          valitseResepti(reseptit)
        }
      }
    }

    def etsiKotona {
      tila match {
        case "main" => {
          output.text = "Etsit��n kotona olevista ainesosista teht�viss� olevat."
          kysyAllergeeni
          tila = "etsiKotona1"
        }
        case "etsiKotona1" => {
          allergeeniT = teksti
          output.append("\nAllergeeni: " + allergeeniT + "\nSy�t� maksimim��r� puuttuvia ainesosia.")
          tila = "etsiKotona2"
        }
        case "etsiKotona2" => {
          maaraT = teksti
          val maara = if (maaraT.isEmpty()) 0 else maaraT.toInt
          val reseptit = reseptikirja.etsija.varastossa(maara, reseptikirja.reseptit, reseptikirja.varasto, allergeeniT)
          valitseResepti(reseptit)
        }
      }
    }

    def lisaaResepti {
      tila match {
        case "main" => {
          output.text = "Luodaan uusi resepti.\nSy�t� reseptin nimi."
          input.requestFocus()
          tila = "lisaaResepti1"
        }
        case "lisaaResepti1" => {
          nimiT = teksti
          output.append("\nReseptin nimi on " + nimiT + "\nSy�t� aineen nimi. Kun kaikki aineet on lis�tty, sy�t� tyhj� rivi.")
          tila = "lisaaReseptiAine"
        }
        case "lisaaReseptiAine" => {
          if (teksti.isEmpty()) {
            output.text = "Sy�t� ohje.(Huom. ei rivinvaihtoja!)"
            tila = "lisaaReseptiOhje"
          } else {
            aineNimiT = teksti
            output.text = "Sy�t� m��r� ilman yksikk��."
            tila = "lisaaReseptiMaara"
          }
        }
        case "lisaaReseptiMaara" => {
          maaraT = teksti
          output.text = "Sy�t� yksikk�."
          tila = "lisaaReseptiYksikko"
        }
        case "lisaaReseptiYksikko" => {
          yksikkoT = teksti
          output.text = "Sy�t� seuraavan aineen nimi, tai lopuksi tyhj� rivi."
          tila = "lisaaReseptiAine"
          aineetT += ((aineNimiT, maaraT.toDouble, yksikkoT))
        }
        case "lisaaReseptiOhje" => {
          ohjeT = teksti
          reseptikirja.lisaaResepti(nimiT, aineetT.toVector, ohjeT)
          output.text = "Resepti lis�tty."
          nollaaTila()
        }

      }
    }

    def poistaResepti {
      tila match {
        case "main" => {
          output.text = "Sy�t� poistettavan reseptin nimi."
          tila = "poistaResepti1"
        }
        case "poistaResepti1" => {
          if (reseptikirja.reseptit.contains(teksti)) {
            reseptikirja.poistaResepti(teksti)
            output.text = "Resepti " + teksti + " poistettu onnistuneesti."
            nollaaTila()
          } else {
            output.text = "Resepti� " + teksti + " ei l�ydetty!"
            nollaaTila()
          }
        }
      }
    }

    def tarkistaVarasto {
      output.text = "Varaston sis�lt�:\nnimi|m��r�|yksikk�|tiheys(g/l)|allergeeni\n"
      reseptikirja.varasto.values.foreach { x => output.append(x.toString() + "\n") }
    }

    def lisaaVarastoon {
      tila match {
        case "main" => {
          input.requestFocus()
          output.text = "Lis�t��n aine varastoon.\nSy�t� aineen nimi."
          tila = "lisaaVarastoon1"
        }
        case "lisaaVarastoon1" => {
          nimiT = teksti.toLowerCase()
          output.append("\nSy�t� aineen m��r� ilman yksikk��.")
          tila = "lisaaVarastoon2"
        }
        case "lisaaVarastoon2" => {
          maaraT = teksti
          output.append("\nSy�t� yksikk�")
          tila = "lisaaVarastoon3"
        }
        case "lisaaVarastoon3" => {
          yksikkoT = teksti.toLowerCase()
          if (reseptikirja.varasto.contains(nimiT.toLowerCase())) {
            val vanhaAine = reseptikirja.varasto(nimiT.toLowerCase())
            val rivi = nimiT + ", " + maaraT + ", " + yksikkoT + ", " + vanhaAine.tiheys + ", " + vanhaAine.allergeeni
            reseptikirja.lisaaVarastoon(rivi)
            output.text = "Ainetta lis�tty onnistuneesti."
            nollaaTila()
          } else {
            output.append("\nSy�t� tiheys(g/l)")
            tila = "lisaaVarastoon4"
          }
        }
        case "lisaaVarastoon4" => {
          tiheysT = teksti.toLowerCase()
          output.append("\nSy�t� mahdollinen allergeeni, muuten tyhj� rivi")
          tila = "lisaaVarastoon5"
        }
        case "lisaaVarastoon5" => {
          val rivi = nimiT + ", " + maaraT + ", " + yksikkoT + ", " + tiheysT + ", " + teksti
          reseptikirja.lisaaVarastoon(rivi)
          output.text = "Aine lis�tty onnistuneesti."
          nollaaTila()
        }
      }
    }

    def vahennaVarastosta {
      tila match {
        case "main" => {
          input.requestFocus()
          output.text = "Sy�t� v�hennett�v�n aineen nimi."
          tila = "vahennaVarastosta1"
        }
        case "vahennaVarastosta1" => {
          nimiT = teksti.toLowerCase()
          output.append("\nSy�t� v�hennett�v� m��r� ilman yksikk��.")
          tila = "vahennaVarastosta2"
        }
        case "vahennaVarastosta2" => {
          maaraT = teksti
          output.append("\nSy�t� yksikk�.")
          tila = "vahennaVarastosta3"
        }
        case "vahennaVarastosta3" => {
          yksikkoT = teksti.toLowerCase()
          if (reseptikirja.varasto.contains(nimiT)) {
            val rivi = nimiT + ", " + maaraT.toDouble + ", " + yksikkoT
            reseptikirja.vahennaVarastosta(rivi)
            output.text = "Ainetta " + nimiT + " v�hennetty onnistuneesti."
            nollaaTila()
          } else {
            output.text = "Ainetta " + nimiT.toLowerCase() + " ei l�ydetty!"
            nollaaTila()
          }
        }
      }
    }

    def poistaVarastosta {
      tila match {
        case "main" => {
          input.requestFocus()
          output.text = "Sy�t� poistettavan aineen nimi."
          tila = "poistaVarastosta1"
        }
        case "poistaVarastosta1" => {
          if (reseptikirja.varasto.contains(teksti.toLowerCase())) {
            reseptikirja.poistaAine(teksti.toLowerCase())
            output.text = "Aine " + teksti.toLowerCase() + " poistettu onnistuneesti."
            nollaaTila()
          } else {
            output.text = "Ainetta " + teksti.toLowerCase() + " ei l�ydetty!"
            nollaaTila()
          }
        }
      }
    }

    def kysyAllergeeni = {
      output.append("\nSy�t� ensin poisj�tett�v� allergeeni tai tyhj� rivi.")
      input.requestFocus()
    }

    def valitseResepti(reseptit: Buffer[Resepti]) {
      tila match {
        case "valitse2" => {
          output.text = reseptikirja.reseptit(teksti).toString()
          nollaaTila()
        }
        case _ => {
          if (reseptit.isEmpty) {
            output.text = "Yht��n resepti� ei l�ytynyt nimell� " + nimiT
            nollaaTila()
          } else if (reseptit.size == 1) {
            val resepti = reseptit(0)
            var tuloste = resepti.toString()
            output.text = tuloste
            nollaaTila()
          } else {
            output.text = "Useita reseptej� l�ytyi. Sy�t� haluamasi reseptin nimi.\n"
            reseptit.foreach { x => output.append(x.nimi + "\n") }
            reseptitT = reseptit
            tila = "valitse2"
          }
        }
      }

    }

    def nollaaTila() {
      teksti = ""
      allergeeniT = ""
      nimiT = ""
      maaraT = ""
      yksikkoT = ""
      tiheysT = ""
      ohjeT = ""
      tila = "main"
      reseptitT = Buffer.empty[Resepti]
    }

  }

  val top = frame

}

