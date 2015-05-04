
package reseptikirja

import scala.collection.mutable._
/**
 * @author Rasmus
 */
class Reseptikirja {
  //Luodaan tiedostojen käsittelijät
  val varastonLukija = new VarastoLukija
  val reseptinLukija = new ReseptiLukija
  val muunnos = new Muunnos
  val etsija = new ReseptinEtsija

  //Haetaan reseptit sekä varaston sisältö.
  var varasto = varastonLukija.lataaVarasto.map(x => x.nimi -> x).toMap
  var reseptit = reseptinLukija.lataaReseptit.map(x => x.nimi -> x).toMap

  //Luodaan toiminnallisuutta
  def tallennaVarasto = varastonLukija.tallennaVarasto(varasto.values.toBuffer)
  def tallennaReseptit = reseptinLukija.tallennaReseptit(reseptit.values.toBuffer)

  def lisaaVarastoon(rivi: String) = {
    var uusiAine = varastonLukija.muodostaAine(rivi)
    if (varasto.keys.toBuffer.contains(uusiAine.nimi)) {
      varasto(uusiAine.nimi).maara = muunnos.lisaaVarastoon(uusiAine.maara, uusiAine.yksikko,
        varasto(uusiAine.nimi).maara, varasto(uusiAine.nimi).yksikko, varasto(uusiAine.nimi).tiheys)
    } else varasto += uusiAine.nimi -> uusiAine
  }

  def vahennaVarastosta(rivi: String) = {
    val nimi = rivi.replaceAll(" ", "").split(",")(0)
    val maara = rivi.replaceAll(" ", "").split(",")(1).toDouble
    val yksikko = rivi.replaceAll(" ", "").split(",")(2)
    if (varasto.keys.toBuffer.contains(nimi)) {
      varasto(nimi).maara = muunnos.vahennaVarastosta(maara, yksikko,
        varasto(nimi).maara, varasto(nimi).yksikko, varasto(nimi).tiheys)
    }
  }
  
  def poistaAine(nimi: String) = if(varasto.contains(nimi)) varasto -= nimi

  
  def lisaaResepti(nimi: String, aineet: Vector[(String, Double, String)], ohje: String): Unit = {
    if(nimi.isEmpty()) return
    val resepti = new Resepti(nimi, aineet, ohje)
    if (!reseptit.contains(resepti.nimi)) reseptit += resepti.nimi -> resepti
  }
  
  def poistaResepti(nimi: String) ={
    if(reseptit.contains(nimi)) reseptit -= nimi
  }

}
  
  
  
  
