package reseptikirja

class Muunnos {
  
  
  
  
  def lisaaVarastoon(lisattavaMaara: Double, lisattavaYksikko: String, varastoMaara: Double, varastoYksikko: String, tiheys: Int): Double = {
      varastoMaara+palautaOikeassaYksikossa(lisattavaYksikko, lisattavaMaara, varastoYksikko, tiheys)
  }
  def vahennaVarastosta(vahennettavaMaara: Double, vahennettavaYksikko: String, varastoMaara: Double, varastoYksikko: String, tiheys: Int): Double = {
      val muunnettuMaara = palautaOikeassaYksikossa(vahennettavaYksikko, vahennettavaMaara, varastoYksikko, tiheys)
      if(muunnettuMaara > varastoMaara) return 0.0
      else varastoMaara-muunnettuMaara
  }

  def palautaOikeassaYksikossa(lisattavaYksikko: String, lisattavaMaara: Double, varastoYksikko: String, tiheys: Int): Double = {
    lisattavaYksikko match {
      case "kg" => varastoYksikko match {
        case "kg" => return lisattavaMaara
        case "g" => return 1000.0 * lisattavaMaara
        case "l" => return lisattavaMaara / (tiheys.toDouble / 1000.0)
        case "dl" => return (lisattavaMaara / (tiheys.toDouble / 1000.0)) * 10.0
        case "rkl" => return (lisattavaMaara / (tiheys.toDouble / 1000.0)) * 1000.0 / 15.0
        case "tl" => return (lisattavaMaara / (tiheys.toDouble / 1000.0)) * 1000.0 / 5.0
        case "mm" => return (lisattavaMaara / (tiheys.toDouble / 1000.0)) * 1000.0
        case _ => return lisattavaMaara
         
      }
      case "g" => varastoYksikko match {
        case "kg" => return lisattavaMaara / 1000
        case "g" => return lisattavaMaara
        case "l" => return lisattavaMaara / tiheys.toDouble
        case "dl" => return (lisattavaMaara / (tiheys.toDouble)) * 10.0
        case "rkl" => return (lisattavaMaara / tiheys.toDouble) * 1000.0 / 15.0
        case "tl" => return (lisattavaMaara / tiheys.toDouble) * 1000.0 / 5.0
        case "mm" => return (lisattavaMaara / tiheys.toDouble) * 1000.0
        case _ => return lisattavaMaara
      }
      case "l" => varastoYksikko match {
        case "kg" => return (tiheys.toDouble * lisattavaMaara) / 1000.0
        case "g" => return (tiheys.toDouble * lisattavaMaara)
        case "l" => return lisattavaMaara
        case "dl" => return lisattavaMaara * 10.0
        case "rkl" => return lisattavaMaara * 1000.0 / 15.0
        case "tl" => return lisattavaMaara * 1000.0 / 5.0
        case "mm" => return lisattavaMaara
        case _ => return lisattavaMaara
      }
      case "dl" => varastoYksikko match {
        case "kg" => return (tiheys.toDouble * lisattavaMaara) / 100.0
        case "g" => return (tiheys.toDouble * lisattavaMaara) / 10.0
        case "l" => return lisattavaMaara / 10.0
        case "dl" => return lisattavaMaara
        case "rkl" => return lisattavaMaara * 100.0 / 15.0
        case "tl" => return lisattavaMaara * 100.0 / 5.0
        case "mm" => return lisattavaMaara * 100.0
        case _ => return lisattavaMaara
      }
      case "rkl" => varastoYksikko match {
        case "kg" => return (tiheys.toDouble * ((lisattavaMaara * 15.0) / 1000.0)) / 1000.0
        case "g" => return (tiheys.toDouble * ((lisattavaMaara * 15.0) / 1000.0))
        case "l" => return lisattavaMaara * 15.0 / 1000.0
        case "dl" => return lisattavaMaara * 15.0 / 100.0
        case "rkl" => return lisattavaMaara
        case "tl" => return lisattavaMaara * 3
        case "mm" => return lisattavaMaara * 15.0
        case _ => return lisattavaMaara
      }
      case "tl" => varastoYksikko match {
        case "kg" => return (tiheys.toDouble * ((lisattavaMaara * 5.0) / 1000.0)) / 1000.0
        case "g" => return (tiheys.toDouble * ((lisattavaMaara * 5.0) / 1000.0))
        case "l" => return lisattavaMaara * 5.0 / 1000.0
        case "dl" => return lisattavaMaara * 5.0 / 100.0
        case "rkl" => return lisattavaMaara / 3
        case "tl" => return lisattavaMaara
        case "mm" => return lisattavaMaara * 5.0
        case _ => return lisattavaMaara
      }
      case "mm" => varastoYksikko match {
        case "kg" => return (tiheys.toDouble * (lisattavaMaara / 1000.0)) / 1000.0
        case "g" => return (tiheys.toDouble * (lisattavaMaara / 1000.0))
        case "l" => return lisattavaMaara / 1000.0
        case "dl" => return lisattavaMaara / 100.0
        case "rkl" => return lisattavaMaara / 15.0
        case "tl" => return lisattavaMaara / 5.0
        case "mm" => return lisattavaMaara
        case _ => return lisattavaMaara
      }
      case "kpl" => return lisattavaMaara
      case _ => return lisattavaMaara
    }
  }
}