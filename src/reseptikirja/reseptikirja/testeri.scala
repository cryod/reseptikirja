package reseptikirja




object testeri extends App {

  val varastonLukija = new VarastoLukija
  val reseptinLukija = new ReseptiLukija
  val a = varastonLukija.lataaVarasto
  a.foreach {x => println(x) }
  val b = reseptinLukija.lataaReseptit
  b.foreach { x => println(x) }
  
//  var c = Vector(("muna", 1.0, "kpl"))
//  b += new Resepti("asdf", c, "lolleroo" )
//  reseptinLukija.tallennaReseptit(b)
  a += new Aine("muna", "kpl", 3, 1200,"")
  varastonLukija.tallennaVarasto(a)
  
  
}