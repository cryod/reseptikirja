package reseptikirja


object testeri extends App {

  val varastonLukija = new VarastoLukija
  println("wtf")
  val a = varastonLukija.lataaVarasto
  a.foreach { println(_) }
}