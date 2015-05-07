
package reseptikirja

import scala.collection.mutable._
/**
 * @author Rasmus
 */
class Reseptikirja {
  //Luodaan tiedostojen k�sittelij�t
  val varastonLukija = new VarastoLukija
  val reseptinLukija = new ReseptiLukija
  val muunnos = new Muunnos
  val etsija = new ReseptinEtsija

  //Haetaan reseptit sek� varaston sis�lt�.
  var varasto = varastonLukija.lataaVarasto.map(x => x.nimi -> x).toMap
  var reseptit = reseptinLukija.lataaReseptit.map(x => x.nimi -> x).toMap

  //Tallennusmetodit
  def tallennaVarasto = varastonLukija.tallennaVarasto(varasto.values.toBuffer)
  def tallennaReseptit = reseptinLukija.tallennaReseptit(reseptit.values.toBuffer)
  
  // Aineen lis��mismetodi
  // Ottaa vastaan rivin muotoa "nimi, m��r�, yksikk�, tiheys, allergeeni"
  // Jos varastossa on jo ainetta, lis�t��n vain m��r��, muuten luodaan uusi aine.
  def lisaaVarastoon(rivi: String) = {
    var uusiAine = varastonLukija.muodostaAine(rivi)
    if (varasto.keys.toBuffer.contains(uusiAine.nimi)) {
      varasto(uusiAine.nimi).maara = muunnos.lisaaVarastoon(uusiAine.maara, uusiAine.yksikko,
        varasto(uusiAine.nimi).maara, varasto(uusiAine.nimi).yksikko, varasto(uusiAine.nimi).tiheys)
    } else varasto += uusiAine.nimi -> uusiAine
  }
  
  // Aineen m��r�n v�hennysmetodi
  // Ottaa vasteaan rivin muotoa "nimi, m��r�, yksikk�"
  // Jos ainetta ei l�ydy, ei tehd� mit��n. Ei voi v�hent�� m��r�� alle 0
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

  // Lis�� reseptin. Jos saman niminen resepti l�ytyy jo, ei tee mit��n.
  def lisaaResepti(nimi: String, aineet: Vector[(String, Double, String)], ohje: String): Unit = {
    if(nimi.isEmpty()) return
    val resepti = new Resepti(nimi, aineet, ohje)
    if (!reseptit.contains(resepti.nimi)) reseptit += resepti.nimi -> resepti
  }
  
  // Poistaa reseptin. Ellei resepti� ole listassa ei tee mit��n.
  def poistaResepti(nimi: String) ={
    if(reseptit.contains(nimi)) reseptit -= nimi
  }

}
  
  
  
  
