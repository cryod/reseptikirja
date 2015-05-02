package reseptikirja




object testeri extends App {

//  val varastonLukija = new VarastoLukija
//  val reseptinLukija = new ReseptiLukija
//  val muunnos = new Muunnos
//  var varasto = varastonLukija.lataaVarasto
//  varasto.foreach {x => println(x) }
//  var reseptit = reseptinLukija.lataaReseptit
//  reseptit.foreach { x => println(x) }
  
  
//  var c = Vector(("muna", 1.0, "kpl"))
//  b += new Resepti("asdf", c, "lolleroo" )
//  reseptinLukija.tallennaReseptit(b)
//  a += new Aine("muna", "kpl", 3, 1200,"")
//  varastonLukija.tallennaVarasto(a)
  
  val kirja = new Reseptikirja
  
  kirja.varasto.values.foreach { println(_)}
  println()
  kirja.lisaaVarastoon("jauho, 2.0, kg, 650")
  kirja.varasto.values.foreach { println(_)}
  println()
  kirja.vahennaVarastosta("jauho, 2100.0, g, 650")
  kirja.varasto.values.foreach { println(_)}
  
  
  
  
}