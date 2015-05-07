
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

  //Tallennusmetodit
  def tallennaVarasto = varastonLukija.tallennaVarasto(varasto.values.toBuffer)
  def tallennaReseptit = reseptinLukija.tallennaReseptit(reseptit.values.toBuffer)
  
  // Aineen lisäämismetodi
  // Ottaa vastaan rivin muotoa "nimi, määrä, yksikkö, tiheys, allergeeni"
  // Jos varastossa on jo ainetta, lisätään vain määrää, muuten luodaan uusi aine.
  def lisaaVarastoon(rivi: String) = {
    var uusiAine = varastonLukija.muodostaAine(rivi)
    if (varasto.keys.toBuffer.contains(uusiAine.nimi)) {
      varasto(uusiAine.nimi).maara = muunnos.lisaaVarastoon(uusiAine.maara, uusiAine.yksikko,
        varasto(uusiAine.nimi).maara, varasto(uusiAine.nimi).yksikko, varasto(uusiAine.nimi).tiheys)
    } else varasto += uusiAine.nimi -> uusiAine
  }
  
  // Aineen määrän vähennysmetodi
  // Ottaa vasteaan rivin muotoa "nimi, määrä, yksikkö"
  // Jos ainetta ei löydy, ei tehdä mitään. Ei voi vähentää määrää alle 0
  def vahennaVarastosta(rivi: String) = {
    val nimi = rivi.replaceAll(" ", "").split(",")(0)
    val maara = rivi.replaceAll(" ", "").split(",")(1).toDouble
    val yksikko = rivi.replaceAll(" ", "").split(",")(2)
    if (varasto.keys.toBuffer.contains(nimi)) {
      varasto(nimi).maara = muunnos.vahennaVarastosta(maara, yksikko,
        varasto(nimi).maara, varasto(nimi).yksikko, varasto(nimi).tiheys)
    }
  }
  
  // Poistaa aineen kokonaan varastosta
  def poistaAine(nimi: String) = if(varasto.contains(nimi)) varasto -= nimi

  // Lisää reseptin. Jos saman niminen resepti löytyy jo, ei tee mitään.
  def lisaaResepti(nimi: String, aineet: Vector[(String, Double, String)], ohje: String): Unit = {
    if(nimi.isEmpty()) return
    val resepti = new Resepti(nimi, aineet, ohje)
    if (!reseptit.contains(resepti.nimi)) reseptit += resepti.nimi -> resepti
  }
  
  // Poistaa reseptin. Ellei reseptiä ole listassa ei tee mitään.
  def poistaResepti(nimi: String) ={
    if(reseptit.contains(nimi)) reseptit -= nimi
  }

}
  
  
  
  
