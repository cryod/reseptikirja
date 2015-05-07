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
    // Ikkunan ominaisuudet kuntoon
    title = "Reseptikirja"
    minimumSize = new Dimension(550, 430)
    preferredSize = new Dimension(600, 500)
    resizable = true
    visible = true
    peer.setLocationRelativeTo(null)

    // Alustetaan kirja
    val reseptikirja = new Reseptikirja
    // V‰liaikaisia muuttujia syˆtteit‰ varten
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

    // Tehd‰‰n yl‰valikko, johon ohjelman sukeminen ja reseptien/varaston tallentaminen
    this.menuBar = new MenuBar {
      contents += new Menu("Ohjelma") {
        val tallennaReseptit = Action("Tallenna reseptit") {
          reseptikirja.tallennaReseptit
        }
        val (tallennaVarasto) = Action("Tallenna varasto") {
          reseptikirja.tallennaVarasto
        }
        val sammuta = Action("Sulje") {
          reseptikirja.tallennaReseptit
          reseptikirja.tallennaVarasto
          dispose()
        }
        contents += new MenuItem(tallennaReseptit)
        contents += new MenuItem(tallennaVarasto)
        contents += new MenuItem(sammuta)
      }
    }
    // Ikkunan sis‰lle paneeli johon asiat tulee
    val sisalto = new BorderPanel()
    // Tulostelaatikko
    val output = new TextArea(35, 50) {
      editable = false
      wordWrap = true
      lineWrap = true
      text = "Tervetuloa k‰ytt‰m‰‰n reseptikirjaa.\nValitse haluamasi toiminto vasemmalta laidalta, ja seuraa ohjeita n‰ytˆll‰."
    }
    // Input laatikko ja sille kuuntelu
    val input = new TextField(20)
    this.listenTo(input.keys)

    // Tehd‰‰n kaikki napit ja niille kuuntelut

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
    lisaaReseptiNappi.text = "Lis‰‰ resepti"
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
    lisaaVarastoonNappi.text = "Lis‰‰ varastoon"
    listenTo(lisaaVarastoonNappi)

    val vahennaVarastostaNappi = new Button
    vahennaVarastostaNappi.background = java.awt.Color.lightGray
    vahennaVarastostaNappi.text = "V‰henn‰ varastosta"
    listenTo(vahennaVarastostaNappi)

    val poistaVarastostaNappi = new Button
    poistaVarastostaNappi.background = java.awt.Color.lightGray
    poistaVarastostaNappi.text = "Poista varastosta"
    listenTo(poistaVarastostaNappi)

    // Paneeli napeille ja lis‰t‰‰n napit sinne.
    // Mukana pari tyhj‰‰ labelia jotta erottelu onnistuu n‰timmin
    val napit = new BoxPanel(swing.Orientation.Vertical)
    napit.visible = true
    napit.contents += new Label(" ")
    napit.contents += new Label("Etsi reseptej‰")
    napit.contents += etsiNimiNappi
    napit.contents += etsiKotonaNappi
    napit.contents += etsiAinesNappi
    napit.contents += new Label(" ")
    napit.contents += new Label("Hallinnoi reseptej‰")
    napit.contents += lisaaReseptiNappi
    napit.contents += poistaReseptiNappi
    napit.contents += new Label(" ")
    napit.contents += new Label("Hallinnoi varastoa")
    napit.contents += tarkistaVarastoNappi
    napit.contents += lisaaVarastoonNappi
    napit.contents += vahennaVarastostaNappi
    napit.contents += poistaVarastostaNappi

    // Tehd‰‰n viel‰ koko kuntoon
    val dimensio = new Dimension(200, 25)
    for (x <- napit.contents) {
      x.preferredSize = dimensio
      x.minimumSize = dimensio
      x.maximumSize = dimensio
    }

    // Asetellaan asiat paikoilleen
    sisalto.layout(napit) = BorderPanel.Position.West
    sisalto.layout(output) = BorderPanel.Position.Center
    sisalto.layout(input) = BorderPanel.Position.South

    // Todetaan ett‰ sis‰ltˆ on mit‰ aikaisemmin lis‰ttiin
    contents = sisalto

    // Tehd‰‰n reaktiot kuntoon.
    reactions += {
      // Napeista ohjataan oikeaan metodiin tekstin perusteella. Tila nollataan ennen eteenp‰inmenoa
      // jotta ei tule logiikkakonflikteja
      case click: ButtonClicked =>
        val lahde = click.source
        nollaaTila()
        lahde.text match {
          case "Etsi nimen perusteella" => etsiNimella
          case "Etsi kotona olevista aineista" => etsiKotona
          case "Etsi ainesosan perusteella" => etsiAine
          case "Lis‰‰ resepti" => lisaaResepti
          case "Poista resepti" => poistaResepti
          case "Tarkista varastotilanne" => tarkistaVarasto
          case "Lis‰‰ varastoon" => lisaaVarastoon
          case "V‰henn‰ varastosta" => vahennaVarastosta
          case "Poista varastosta" => poistaVarastosta
          case _ =>
        }
      // Entterin painalluksista reagoidaan tilan mukaan ohjaten oikeaan metodiin
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

    // --- erottaa caset toisistaan

    // Seuraavat metodit toimivat seuraavanlaisella logiikalla:
    // Klikataan nappia (esim. etsi nimen perusteella) -> reaktioista siirryt‰‰n metodiin etsiNimella
    // etsiNimell‰ matchaa tilan ensin "main" -> tehd‰‰n asiat, lopuksi vaihdetaan tilaksi "etsiNimella1"
    // K‰ytt‰j‰ syˆtt‰‰ tekstin ja painaa entteri‰ -> reaktioista p‰‰dyt‰‰n takaisin etsiNimella metodiin
    // tila matchataan "etsiNimella1" -> tehd‰‰n asiat, lopuksi tila = "etsiNimella2"
    // Taas k‰ytt‰j‰n syˆte ja enter -> reaktioista p‰‰dyt‰‰n taas etsiNimella metodiin
    // match "etsiNimella2" -> tehd‰‰n asiat, tilaksi tulee "valitse" ja kutsutaan valitseResepti metodia
    // valitseReseptit matchaa taas tilan ja tekee toimintonsa. Kun t‰m‰ tie on saatu loppuun ja toiminnot tehty,
    // kutsutaan nollaaTila() joka tyhjent‰‰ v‰liaikaiset muuttujat ja palauttaa tilaksi "main"
    // T‰ten ollaan taas alkutilanteessa.

    // Metodien tapahtumat ovat suoraviivaisi, niiss‰ kutsutaan taustalla olevien luokkien metodeita kun ollaan ensin
    // ker‰tty tarvittavat syˆtteet. N‰ist‰ saadut tulokset sitten tulostetaan tarvittaessa.

    // Etsit‰‰n reseptin nimell‰.
    def etsiNimella {
      tila match {
        //---------------------------------------------------------------------------------------------------------- 
        case "main" => {
          output.text = "Etsi reseptin nimell‰."
          kysyAllergeeni
          tila = "etsiNimella1"
        } //---------------------------------------------------------------------------------------------------------- 
        case "etsiNimella1" => {
          allergeeniT = teksti
          output.append("\nAllergeeni: " + allergeeniT + "\nSyˆt‰ etsitt‰v‰ nimi.")
          tila = "etsiNimella2"
        } //----------------------------------------------------------------------------------------------------------
        case "etsiNimella2" => {
          nimiT = teksti
          val reseptit = reseptikirja.etsija.nimi(nimiT, reseptikirja.reseptit, reseptikirja.varasto, allergeeniT)
          tila = "valitse"
          valitseResepti(reseptit)
        } //----------------------------------------------------------------------------------------------------------
      }
    }

    def etsiAine {
      tila match {
        //----------------------------------------------------------------------------------------------------------
        case "main" => {
          output.text = "Etsi k‰ytett‰v‰ll‰ ainesosalla."
          kysyAllergeeni
          tila = "etsiAine1"
        } //----------------------------------------------------------------------------------------------------------
        case "etsiAine1" => {
          allergeeniT = teksti
          output.append("\nAllergeeni: " + allergeeniT + "\nSyˆt‰ etsitt‰v‰n ainesosan nimi.")
          tila = "etsiAine2"
        } //----------------------------------------------------------------------------------------------------------
        case "etsiAine2" => {
          nimiT = teksti
          val reseptit = reseptikirja.etsija.ainesOsa(nimiT, reseptikirja.reseptit, reseptikirja.varasto, allergeeniT)
          tila = "valitse"
          valitseResepti(reseptit)
        } //----------------------------------------------------------------------------------------------------------
      }
    }

    def etsiKotona {
      tila match {
        //----------------------------------------------------------------------------------------------------------
        case "main" => {
          output.text = "Etsit‰‰n kotona olevista ainesosista teht‰viss‰ olevat."
          kysyAllergeeni
          tila = "etsiKotona1"
        } //----------------------------------------------------------------------------------------------------------
        case "etsiKotona1" => {
          allergeeniT = teksti
          output.append("\nAllergeeni: " + allergeeniT + "\nSyˆt‰ maksimim‰‰r‰ puuttuvia ainesosia.")
          tila = "etsiKotona2"
        } //----------------------------------------------------------------------------------------------------------
        case "etsiKotona2" => {
          maaraT = teksti
          val maara = if (maaraT.isEmpty()) 0 else maaraT.toInt
          val reseptit = reseptikirja.etsija.kotona(maara, reseptikirja.reseptit, reseptikirja.varasto, allergeeniT, reseptikirja.muunnos)
          valitseResepti(reseptit)
        } //----------------------------------------------------------------------------------------------------------
      }
    }

    def lisaaResepti {
      tila match {
        //----------------------------------------------------------------------------------------------------------
        case "main" => {
          output.text = "Luodaan uusi resepti.\nSyˆt‰ reseptin nimi."
          input.requestFocus()
          tila = "lisaaResepti1"
        } //----------------------------------------------------------------------------------------------------------
        case "lisaaResepti1" => {
          nimiT = teksti.trim()
          if (reseptikirja.reseptit.contains(nimiT)) {
            output.text = "Resepti " + nimiT + " on jo olemassa."
            tila = "main"
          } else {
            output.append("\nReseptin nimi on " + nimiT + "\nSyˆt‰ aineen nimi. Kun kaikki aineet on lis‰tty, syˆt‰ tyhj‰ rivi.")
            tila = "lisaaReseptiAine"
          }
        } //----------------------------------------------------------------------------------------------------------
        case "lisaaReseptiAine" => {
          if (teksti.isEmpty()) {
            output.text = "Syˆt‰ ohje.(Huom. ei rivinvaihtoja!)"
            tila = "lisaaReseptiOhje"
          } else {
            aineNimiT = teksti.trim()
            output.text = "Syˆt‰ m‰‰r‰ ilman yksikkˆ‰."
            tila = "lisaaReseptiMaara"
          }
        } //----------------------------------------------------------------------------------------------------------
        case "lisaaReseptiMaara" => {
          maaraT = teksti
          output.text = "Syˆt‰ yksikkˆ.\nMahdolliset yksikˆt: g, kg, l, dl, rkl, tl, mm"
          tila = "lisaaReseptiYksikko"
        } //----------------------------------------------------------------------------------------------------------
        case "lisaaReseptiYksikko" => {
          yksikkoT = teksti.trim()
          output.text = "Syˆt‰ seuraavan aineen nimi, tai lopuksi tyhj‰ rivi."
          tila = "lisaaReseptiAine"
          aineetT += ((aineNimiT, maaraT.toDouble, yksikkoT))
        } //----------------------------------------------------------------------------------------------------------
        case "lisaaReseptiOhje" => {
          ohjeT = teksti.trim()
          reseptikirja.lisaaResepti(nimiT, aineetT.toVector, ohjeT)
          output.text = "Resepti lis‰tty."
          nollaaTila()
        } //----------------------------------------------------------------------------------------------------------

      }
    }

    def poistaResepti {
      tila match {
        //----------------------------------------------------------------------------------------------------------
        case "main" => {
          output.text = "Syˆt‰ poistettavan reseptin nimi."
          tila = "poistaResepti1"
        } //----------------------------------------------------------------------------------------------------------
        case "poistaResepti1" => {
          if (reseptikirja.reseptit.contains(teksti)) {
            reseptikirja.poistaResepti(teksti)
            output.text = "Resepti " + teksti + " poistettu onnistuneesti."
            nollaaTila()
          } else {
            output.text = "Resepti‰ " + teksti + " ei lˆydetty!"
            nollaaTila()
          }
        } //----------------------------------------------------------------------------------------------------------
      }
    }

    def tarkistaVarasto {
      output.text = "Varaston sis‰ltˆ:\nnimi | m‰‰r‰ | yksikkˆ | tiheys(g/l) | allergeeni\n"
      reseptikirja.varasto.values.foreach { x => output.append(x.toString() + "\n") }
    }

    def lisaaVarastoon {
      tila match {
        //----------------------------------------------------------------------------------------------------------
        case "main" => {
          input.requestFocus()
          output.text = "Lis‰t‰‰n aine varastoon.\nSyˆt‰ aineen nimi."
          tila = "lisaaVarastoon1"
        } //----------------------------------------------------------------------------------------------------------
        case "lisaaVarastoon1" => {
          nimiT = teksti.toLowerCase()
          output.append("\nSyˆt‰ aineen m‰‰r‰ ilman yksikkˆ‰.")
          tila = "lisaaVarastoon2"
        } //----------------------------------------------------------------------------------------------------------
        case "lisaaVarastoon2" => {
          maaraT = teksti
          output.append("\nSyˆt‰ yksikkˆ.\nMahdolliset yksikˆt: g, kg, l, dl, rkl, tl, mm")
          tila = "lisaaVarastoon3"
        } //----------------------------------------------------------------------------------------------------------
        case "lisaaVarastoon3" => {
          yksikkoT = teksti.toLowerCase()
          if (reseptikirja.varasto.contains(nimiT.toLowerCase())) {
            val vanhaAine = reseptikirja.varasto(nimiT.toLowerCase())
            val rivi = nimiT + ", " + maaraT + ", " + yksikkoT + ", " + vanhaAine.tiheys + ", " + vanhaAine.allergeeni
            reseptikirja.lisaaVarastoon(rivi)
            output.text = "Ainetta lis‰tty onnistuneesti."
            nollaaTila()
          } else {
            output.append("\nSyˆt‰ tiheys(g/l)")
            tila = "lisaaVarastoon4"
          }
        } //----------------------------------------------------------------------------------------------------------
        case "lisaaVarastoon4" => {
          tiheysT = teksti.toLowerCase()
          output.append("\nSyˆt‰ mahdollinen allergeeni, muuten tyhj‰ rivi.")
          tila = "lisaaVarastoon5"
        } //----------------------------------------------------------------------------------------------------------
        case "lisaaVarastoon5" => {
          val rivi = nimiT + ", " + maaraT + ", " + yksikkoT + ", " + tiheysT + ", " + teksti
          reseptikirja.lisaaVarastoon(rivi)
          output.text = "Aine lis‰tty onnistuneesti."
          nollaaTila()
        } //----------------------------------------------------------------------------------------------------------
      }
    }

    def vahennaVarastosta {
      tila match {
        //----------------------------------------------------------------------------------------------------------
        case "main" => {
          input.requestFocus()
          output.text = "Syˆt‰ v‰hennett‰v‰n aineen nimi."
          tila = "vahennaVarastosta1"
        } //----------------------------------------------------------------------------------------------------------
        case "vahennaVarastosta1" => {
          nimiT = teksti.toLowerCase()
          output.append("\nSyˆt‰ v‰hennett‰v‰ m‰‰r‰ ilman yksikkˆ‰.")
          tila = "vahennaVarastosta2"
        } //----------------------------------------------------------------------------------------------------------
        case "vahennaVarastosta2" => {
          maaraT = teksti
          output.append("\nSyˆt‰ yksikkˆ.\nMahdolliset yksikˆt: g, kg, l, dl, rkl, tl, mm")
          tila = "vahennaVarastosta3"
        } //----------------------------------------------------------------------------------------------------------
        case "vahennaVarastosta3" => {
          yksikkoT = teksti.toLowerCase()
          if (reseptikirja.varasto.contains(nimiT)) {
            val rivi = nimiT + ", " + maaraT.toDouble + ", " + yksikkoT
            reseptikirja.vahennaVarastosta(rivi)
            output.text = "Ainetta " + nimiT + " v‰hennetty onnistuneesti."
            nollaaTila()
          } else {
            output.text = "Ainetta " + nimiT.toLowerCase() + " ei lˆydetty!"
            nollaaTila()
          }
        } //----------------------------------------------------------------------------------------------------------
      }
    }

    def poistaVarastosta {
      tila match {
        //----------------------------------------------------------------------------------------------------------
        case "main" => {
          input.requestFocus()
          output.text = "Syˆt‰ poistettavan aineen nimi."
          tila = "poistaVarastosta1"
        } //----------------------------------------------------------------------------------------------------------
        case "poistaVarastosta1" => {
          if (reseptikirja.varasto.contains(teksti.toLowerCase())) {
            reseptikirja.poistaAine(teksti.toLowerCase())
            output.text = "Aine " + teksti.toLowerCase() + " poistettu onnistuneesti."
            nollaaTila()
          } else {
            output.text = "Ainetta " + teksti.toLowerCase() + " ei lˆydetty!"
            nollaaTila()
          }
        } //----------------------------------------------------------------------------------------------------------
      }
    }

    // Kysyy k‰ytt‰j‰lt‰ allergeenin. Kutsutaan aina ensin reseptej‰ etsitt‰ess‰.
    def kysyAllergeeni = {
      output.append("\nSyˆt‰ ensin poisj‰tett‰v‰ allergeeni tai tyhj‰ rivi.")
      input.requestFocus()
    }

    // Hoitaa hakutuloksista halutun reseptin valitsemisen ja tulostaa lopulta sen.
    def valitseResepti(reseptit: Buffer[Resepti]) {
      tila match {
        //----------------------------------------------------------------------------------------------------------
        case "valitse2" => {
          if(!teksti.isEmpty())output.text = reseptikirja.reseptit(teksti).toString()
          else output.text = "Yksik‰‰n resepti ei vastannut syˆtett‰si "+teksti
          nollaaTila()
        } //----------------------------------------------------------------------------------------------------------
        case _ => {
          if (reseptit.isEmpty) {
            output.text = "Yht‰‰n resepti‰ ei lˆytynyt nimell‰ " + nimiT+"."
            nollaaTila()
          } else if (reseptit.size == 1) {
            val resepti = reseptit(0)
            var tuloste = resepti.toString()
            output.text = tuloste
            nollaaTila()
          } else {
            output.text = "Useita reseptej‰ lˆytyi. Syˆt‰ haluamasi reseptin nimi.\n"
            reseptit.foreach { x => output.append(x.nimi + "\n") }
            reseptitT = reseptit
            tila = "valitse2"
          }
        } //----------------------------------------------------------------------------------------------------------
      }

    }
    
    // Vaihtaa tilaksi "main" ja nollaa kaikki v‰liaikaismuuttujat. N‰in varmistutaan ett‰i jouduta v‰‰r‰‰n paikkaan v‰‰r‰‰n aikaan.
    def nollaaTila() {
      teksti = ""
      allergeeniT = ""
      nimiT = ""
      aineNimiT = ""
      maaraT = ""
      yksikkoT = ""
      tiheysT = ""
      ohjeT = ""
      aineetT = Buffer.empty[(String, Double, String)]
      tila = "main"
      reseptitT = Buffer.empty[Resepti]
    }

  }
  
  val top = frame

}

