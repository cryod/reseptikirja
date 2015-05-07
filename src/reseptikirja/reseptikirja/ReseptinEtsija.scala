package reseptikirja

import scala.collection.mutable._
import scala.collection.immutable._

class ReseptinEtsija {

  // Luokka, jolla hoidetaan reseptien etsiminen.
  // Kaikissa metodeissa otetaan syˆtteeksi muiden lis‰ksi allergeeni, joka voi olla tyhj‰.
  // Jos allergeeni on m‰‰ritelty, tuloksista suodatetaan pois ne reseptit, jotka sis‰lt‰v‰t allergeenia.

  // Etsi reseptin nimen perusteella. K‰y reseptit l‰pi ja tutkii onko sis‰lt‰‰kˆ mink‰‰n nimi hakutermi‰.
  def nimi(nimi: String, reseptit: scala.collection.immutable.Map[String, Resepti], varasto: scala.collection.immutable.Map[String, Aine], allergeeni: String): Buffer[Resepti] = {
    var palautettavat = Buffer.empty[Resepti]
    for (resepti <- reseptit.values) {
      if (resepti.nimi.toLowerCase().contains(nimi.toLowerCase())) palautettavat += resepti
    }
    poistaAllergeenit(palautettavat, allergeeni, varasto)
  }

  // Etsii kotona olevista aineista teht‰viss‰ olevat reseptit.
  // Ottaa syˆtteeksi sallittujen puuttuvien/vajaiden aineiden m‰‰r‰n, joka oletuksena on nolla.
  def kotona(puuttuvienMaara: Int, reseptit: scala.collection.immutable.Map[String, Resepti], varasto: scala.collection.immutable.Map[String, Aine], allergeeni: String, muunnos: Muunnos): Buffer[Resepti] = {
    var palautettavatEka = Buffer.empty[Resepti]
    var palautettavatVajaatEka = Buffer.empty[Resepti]
    var palautettavatToka = Buffer.empty[Resepti]
    var palautettavatVajaatToka = Buffer.empty[Resepti]

    // K‰yd‰‰n kaikki reseptit l‰pi
    for (resepti <- reseptit.values) {
      var aineitaRiittavasti = puuttuvienMaara
      var kaikkiaAineita = puuttuvienMaara

      // K‰yd‰‰n kaikki reseptin aineet l‰pi  
      for (aine <- resepti.aineet) {
        // Tarkistetaan onko reseptin ainetta varastossa ollenkaan.
        if (varasto.contains(aine._1.toLowerCase())) {
          // Tarkistetaan onko sit‰ oikeasti. Jos on, merkataan ett‰ on edes v‰h‰n.
          if (varasto(aine._1.toLowerCase()).maara > 0) {
            kaikkiaAineita += 1
            // Verrataan varastossa olevaa m‰‰r‰‰ ja reseptiss‰ olevaa m‰‰r‰‰ samassa yksikˆss‰. Jos varastossa on tarpeeksi, lis‰t‰‰n merkint‰.
            val varastoMaaraReseptiYksikossa = muunnos.palautaOikeassaYksikossa(varasto(aine._1).yksikko, varasto(aine._1).maara, aine._3, varasto(aine._1).tiheys)
            if (varastoMaaraReseptiYksikossa >= aine._2) aineitaRiittavasti += 1
          }
        }
      }
      // Jos reseptin kaikkia aineita lˆytyy, lis‰t‰‰n vajaiden listaan. Jos kaikkia on tarpeeksi, lis‰t‰‰n myˆs riitt‰vien listaan.
      if (kaikkiaAineita >= resepti.aineet.length) palautettavatVajaatEka += resepti
      if (aineitaRiittavasti >= resepti.aineet.length) palautettavatEka += resepti
    }

    // K‰yd‰‰n toinen kierros, reseptej‰ l‰pi, tarkistaen ett‰ reseptin osana olevia reseptej‰ hyˆtyk‰ytet‰‰n.
    for (resepti <- reseptit.values) {
      var aineitaRiittavasti = puuttuvienMaara
      var kaikkiaAineita = puuttuvienMaara
      for (aine <- resepti.aineet) {
        // Tarkistetaan taas onko varastossa. Jos ei ole, hyp‰t‰‰n seuraavaan kohtaan.
        if (varasto.contains(aine._1.toLowerCase())) {
          kaikkiaAineita += 1
          val varastoMaaraReseptiYksikossa = muunnos.palautaOikeassaYksikossa(varasto(aine._1).yksikko, varasto(aine._1).maara, aine._3, varasto(aine._1).tiheys)
          if (varastoMaaraReseptiYksikossa >= aine._2) aineitaRiittavasti += 1
        } else {
          // K‰yd‰‰n ensimm‰isell‰ kierroksella saadut reseptit l‰pi. 
          for (x <- palautettavatEka) {
            // Jos aikaisemmin on saatu valmis resepti johonkin uuden reseptin aineeseen, k‰ytet‰‰n sit‰.
            if (x.nimi.contains(aine._1)) {
              aineitaRiittavasti += 1
              kaikkiaAineita += 1
            }
          }
        }
      }
      if (kaikkiaAineita >= resepti.aineet.length) palautettavatVajaatToka += resepti
      if (aineitaRiittavasti >= resepti.aineet.length) palautettavatToka += resepti
    }
    // Jos ei saataisi yht‰‰n t‰ytt‰ resepti‰, palautetaan vajaat. Muuten palautetaan vain t‰ydet reseptit.
    if (palautettavatToka.isEmpty) palautettavatVajaatToka
    else palautettavatToka
  }

  // Etsi resepti ainesosan perusteella. Ottaa sis‰‰n etsitt‰v‰n ainesosan nimen.
  def ainesOsa(etsittava: String, reseptit: scala.collection.immutable.Map[String, Resepti], varasto: scala.collection.immutable.Map[String, Aine], allergeeni: String): Buffer[Resepti] = {
    var palautettavat = Buffer.empty[Resepti]
    // K‰yd‰‰n reseptit l‰pi.
    for (resepti <- reseptit.values) {
      var laskuri = 0
      // K‰yd‰‰n reseptin aineet l‰pi. Jos sama aine esiintyy useammin reseptiss‰, ei lis‰t‰ resepti‰ moneen kertaan
      for (aine <- resepti.aineet) if (aine._1.contains(etsittava.toLowerCase())) laskuri += 1
      if (laskuri > 0) palautettavat += resepti
    }
    poistaAllergeenit(palautettavat, allergeeni, varasto)

  }

  
  // Suodattaa palautettavista resepteist‰ pois ne, jotka sis‰lt‰v‰t allergeeni‰. T‰m‰ edellytt‰‰, ett‰ varastossa on edes maininta
  // aineesta, sill‰ reseptit itsess‰‰n eiv‰t sis‰ll‰ huomioita allergeeneist‰. 
  def poistaAllergeenit(suodattamaton: Buffer[Resepti], allergeeni: String, varasto: scala.collection.immutable.Map[String, Aine]): Buffer[Resepti] = {
    var palautettavat = Buffer.empty[Resepti]
    if (allergeeni.isEmpty()) return suodattamaton
    for (resepti <- suodattamaton) {
      var laskuri = 0
      for (x <- resepti.aineet) {
        if (varasto.contains(x._1)) {
          if (varasto(x._1).allergeeni == allergeeni) laskuri += 1
        }
      }
      if (laskuri == 0) palautettavat += resepti
    }
    palautettavat
  }

}