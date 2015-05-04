package reseptikirja

import scala.collection.mutable._
import scala.collection.immutable._

class ReseptinEtsija {
  def nimi(nimi: String, reseptit: scala.collection.immutable.Map[String, Resepti],varasto: scala.collection.immutable.Map[String, Aine], allergeeni: String): Buffer[Resepti] = {
    var palautettavat = Buffer.empty[Resepti]
    for (resepti <- reseptit.values) {
      if (resepti.nimi.contains(nimi)) palautettavat += resepti
    }
    poistaAllergeenit(palautettavat, allergeeni, varasto)
  }
  def varastossa(puuttuvienMaara: Int, reseptit: scala.collection.immutable.Map[String, Resepti], varasto: scala.collection.immutable.Map[String, Aine], allergeeni: String): Buffer[Resepti] = {
  
    val muunnos = new Muunnos
    var palautettavatEka = Buffer.empty[Resepti]
    var palautettavatVajaatEka = Buffer.empty[Resepti]
    var palautettavatToka = Buffer.empty[Resepti]
    var palautettavatVajaatToka = Buffer.empty[Resepti]
    for (resepti <- reseptit.values) {
      var aineitaRiittavasti = puuttuvienMaara
      var kaikkiaAineita = puuttuvienMaara
      for (aine <- resepti.aineet) {
        if (varasto.contains(aine._1)) {
          kaikkiaAineita += 1
          val varastoMaaraReseptiYksikossa = muunnos.palautaOikeassaYksikossa(varasto(aine._1).yksikko, varasto(aine._1).maara, aine._3, varasto(aine._1).tiheys)
          if (varastoMaaraReseptiYksikossa >= aine._2) aineitaRiittavasti += 1
        }
      }
      if (kaikkiaAineita >= resepti.aineet.length) palautettavatVajaatEka += resepti
      if (aineitaRiittavasti >= resepti.aineet.length) palautettavatEka += resepti
    }

    for (resepti <- reseptit.values) {
      var aineitaRiittavasti = puuttuvienMaara
      var kaikkiaAineita = puuttuvienMaara
      for (aine <- resepti.aineet) {
        if (varasto.contains(aine._1)) {
          kaikkiaAineita += 1
          val varastoMaaraReseptiYksikossa = muunnos.palautaOikeassaYksikossa(varasto(aine._1).yksikko, varasto(aine._1).maara, aine._3, varasto(aine._1).tiheys)
          if (varastoMaaraReseptiYksikossa >= aine._2) aineitaRiittavasti += 1
        } else {
          for (x <- palautettavatEka) {
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

    if (palautettavatToka.isEmpty) palautettavatVajaatToka
    else palautettavatToka
  }
  
  def ainesOsa(etsittava: String, reseptit: scala.collection.immutable.Map[String, Resepti], varasto: scala.collection.immutable.Map[String, Aine], allergeeni: String): Buffer[Resepti] = {
    var palautettavat = Buffer.empty[Resepti]
    
    for(resepti<- reseptit.values){
      var laskuri = 0
      for(aine<- resepti.aineet) if(aine._1.contains(etsittava)) laskuri += 1
      if(laskuri>0) palautettavat += resepti
    }
    poistaAllergeenit(palautettavat, allergeeni, varasto)
    
  }
  
  def poistaAllergeenit(suodattamaton: Buffer[Resepti], allergeeni: String, varasto: scala.collection.immutable.Map[String, Aine]):Buffer[Resepti] ={
    var palautettavat = Buffer.empty[Resepti]
    if(allergeeni.isEmpty()) return suodattamaton
    for(resepti<-suodattamaton){
      var laskuri = 0
      for(x<- resepti.aineet){
        if(varasto.contains(x._1)){
          if(varasto(x._1).allergeeni==allergeeni) laskuri += 1
        }
      }
      if(laskuri==0) palautettavat += resepti
    }
    palautettavat
  }

}