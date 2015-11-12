
import scala.util._
import scala.io.Source
import java.io._
import java.nio.charset.CodingErrorAction
import scala.io.Codec

//object Gadchiroli extends WikiFromCensus(districtFile,phrasesFile,sentencesFile)

class WikiFromCensus(districtName: String, language: String){

   def districtFile = "data/"+districtName+".csv"
   def phrasesFile = "data/phrases_"+language+".csv"
   def sentencesFile = "data/sentences_"+language+".csv"
  
  object SentencesPart {
    //lazy val sentences = Source.fromFile("data/Sentences_mr.csv")("UTF-8")
    lazy val sentences = Source.fromFile(sentencesFile)("UTF-8")
    lazy val sentenceLines = sentences.getLines().toArray
    lazy val sentenceArray = sentenceLines map (_.split(",").toArray)
  }
  
    //lazy val src = Source.fromFile("data/Gadchiroli.csv")
    lazy val src = Source.fromFile(districtFile)
    lazy val lines = src.getLines().toList
    lazy val rawtab = lines map (_.split(",").toList)
    lazy val district = rawtab(2)(3).split(":-")(1)

  /* lazy val district = rawtab(2)(3).split(":-")(1) */

  def isInt(s : String) = !Try(s.toInt).toOption.isEmpty

  def hasPeople: String => Boolean = {
    case Number(k) => k >0
    case _ => false
  }

  def isVillage(l: List[String]) = isInt(l(0)) && !isInt(l(1)) &&hasPeople(l(5))

  val tab = rawtab filter (isVillage)

  //lazy val phrases = Source.fromFile("data/Phrases_mr.csv")("UTF-8")
  lazy val phrases = Source.fromFile(phrasesFile)("UTF-8")
  lazy val phraseLines = phrases.getLines().toArray
  lazy val phraseRawTab = phraseLines map (_.split(",").toArray)

  object Number{
    def unapply(s : String) = Try(s.toInt).toOption
  }

  def abc(s : String) = s match {
    case "a" => "< 5 km"
    case "b" => "5-10 kms"
    case "c" => "> 10 km"
    case _ => {println(s"found $s when matching for abc"); "???"}
  }

  def firstPara(s: String, ss: String, t: String, p: String, q: String, r: String, rr: String, desc: String) = {
    val a = desc.replace("[x]", s)
    val b = a.replace("[y]", ss)
    val c = b.replace("[z]", t)
    val d = c.replace("[u]", q)
    val e = d.replace("[v]", p)
    val f = e.replace("[w]", r)
    //val g = f.replace("[ww]", rr)
    
    rr.toLowerCase match {
    case "a" => s"$f ${SentencesPart.sentenceArray(2)(1)}"
    case "b" => s"$f ${SentencesPart.sentenceArray(3)(1)}"
    case "c" => s"$f ${SentencesPart.sentenceArray(4)(1)}"
    case _ => s"$f"
    }
  }
  
  def numOrNearest(s: String, desc: String) = s match {
    case Number(1) => s"${SentencesPart.sentenceArray(0)(0)}1 $desc${SentencesPart.sentenceArray(0)(1)}"
    case Number(k) => s"${SentencesPart.sentenceArray(1)(0)}$k $desc${SentencesPart.sentenceArray(1)(1)}"
    case "a" => s"${SentencesPart.sentenceArray(2)(0)}$desc${SentencesPart.sentenceArray(2)(1)}"
    case "b" => s"${SentencesPart.sentenceArray(3)(0)}$desc${SentencesPart.sentenceArray(3)(1)}"
    case "c" => s"${SentencesPart.sentenceArray(4)(0)}$desc${SentencesPart.sentenceArray(4)(1)}"
    case _ => s" "
  }

  
  def numOrNearest(s: String, desc: String, descs: String) = s match {
    case Number(1) => s"There is 1 $desc in the village"
    case Number(k) => s"There are $k ${descs} in the village"
    case _ => s"The nearest $desc is at a distance of ${abc(s)} from the village."
  }

  def supply(s: String, desc: String) =
    if (s.toLowerCase == "yes") s"${SentencesPart.sentenceArray(5)(0)}$desc${SentencesPart.sentenceArray(5)(1)}"
      else s"${SentencesPart.sentenceArray(6)(0)}$desc${SentencesPart.sentenceArray(6)(1)}"
  
  def available(s: String, desc: String) =
    if (s.toLowerCase == "yes") s"${SentencesPart.sentenceArray(7)(0)}$desc${SentencesPart.sentenceArray(7)(1)}"
      else s"${SentencesPart.sentenceArray(8)(0)}$desc${SentencesPart.sentenceArray(8)(1)}"
      
  def numOrBlank(s: String, desc: String) = s match {
    case Number(1) => s"There is 1 $desc in the village"
    case Number(k) => s"There are $k ${desc}s in the village"
    case _ => ""
  }

  def hasOrNearest(s: String, desc: String) = s.toLowerCase match {
    case "yes" => s"${SentencesPart.sentenceArray(1)(0)}$desc${SentencesPart.sentenceArray(1)(1)}"
    case "a" => s"${SentencesPart.sentenceArray(2)(0)}$desc${SentencesPart.sentenceArray(2)(1)}"
    case "b" => s"${SentencesPart.sentenceArray(3)(0)}$desc${SentencesPart.sentenceArray(3)(1)}"
    case "c" => s"${SentencesPart.sentenceArray(4)(0)}$desc${SentencesPart.sentenceArray(4)(1)}"
    case _ => s" "
  }

  def supplyOrNearest(s: String, desc: String) = s.toLowerCase match {
    case "yes" => s"${SentencesPart.sentenceArray(5)(0)}$desc${SentencesPart.sentenceArray(5)(1)}"
    case "a" => s"${SentencesPart.sentenceArray(2)(0)}$desc${SentencesPart.sentenceArray(2)(1)}"
    case "b" => s"${SentencesPart.sentenceArray(3)(0)}$desc${SentencesPart.sentenceArray(3)(1)}"
    case "c" => s"${SentencesPart.sentenceArray(4)(0)}$desc${SentencesPart.sentenceArray(4)(1)}"
    case _ => s" "
  }
  
  def availableOrNearest(s: String, desc: String) = s.toLowerCase match {
    case "yes" => s"${SentencesPart.sentenceArray(7)(0)}$desc${SentencesPart.sentenceArray(7)(1)}"
    case "no" => s"${SentencesPart.sentenceArray(8)(0)}$desc${SentencesPart.sentenceArray(8)(1)}"
    case "a" => s"${SentencesPart.sentenceArray(2)(0)}$desc${SentencesPart.sentenceArray(2)(1)}"
    case "b" => s"${SentencesPart.sentenceArray(3)(0)}$desc${SentencesPart.sentenceArray(3)(1)}"
    case "c" => s"${SentencesPart.sentenceArray(4)(0)}$desc${SentencesPart.sentenceArray(4)(1)}"
    case _ => s" "
  }

  def connectedOrNearest(s: String, desc: String) = s.toLowerCase match {
    case "yes" => s"${SentencesPart.sentenceArray(9)(0)}$desc${SentencesPart.sentenceArray(9)(1)}"
    case "no" => s"${SentencesPart.sentenceArray(10)(0)}$desc${SentencesPart.sentenceArray(10)(1)}"
    case "a" => s"${SentencesPart.sentenceArray(2)(0)}$desc${SentencesPart.sentenceArray(2)(1)}"
    case "b" => s"${SentencesPart.sentenceArray(3)(0)}$desc${SentencesPart.sentenceArray(3)(1)}"
    case "c" => s"${SentencesPart.sentenceArray(4)(0)}$desc${SentencesPart.sentenceArray(4)(1)}"
    case _ => s" "
  }

  def allPages = tab map (page)

  def manuf(row: List[String]) = {
    val items = List(row(119), row(120), row(121)) filter (_ != "")
    //if (items.isEmpty) ""
    //  else
  s""" ${row(1)} ${phraseRawTab(129)(2)}: ${items.mkString(",")}"""
  }

  def savePage(row: List[String]) = {
    val filename = s"data/${row(1)}_$language.wiki"
    val f = new PrintWriter(filename, "UTF-8")
    f.println(page(row))
    f.close()
  }

  def saveAll() = tab map (savePage)

  def page(row : List[String]) =
  s"""
  ${firstPara(row(1), district, row(3), row(4), row(5), row(101), row(102), phraseRawTab(1)(2))}

== ${phraseRawTab(2)(2)} ==  
${numOrNearest(row(6), phraseRawTab(3)(2))}
${numOrNearest(row(7), phraseRawTab(5)(2))}
${numOrNearest(row(8), phraseRawTab(6)(2))}
${numOrNearest(row(9), phraseRawTab(7)(2))}
${numOrNearest(row(10), phraseRawTab(8)(2))}
${numOrNearest(row(11), phraseRawTab(9)(2))}
${numOrNearest(row(12), phraseRawTab(10)(2))}
${numOrNearest(row(13), phraseRawTab(11)(2))}
${numOrNearest(row(14), phraseRawTab(12)(2))}
${numOrNearest(row(15), phraseRawTab(13)(2))}
${numOrNearest(row(16), phraseRawTab(14)(2))}
${numOrNearest(row(17), phraseRawTab(15)(2))}
${numOrNearest(row(18), phraseRawTab(16)(2))}
${numOrNearest(row(19), phraseRawTab(17)(2))}


== ${phraseRawTab(19)(2)} ==
${numOrNearest(row(20), phraseRawTab(20)(2))}
${numOrNearest(row(21), phraseRawTab(21)(2))}
${numOrNearest(row(22), phraseRawTab(22)(2))}
${numOrNearest(row(23), phraseRawTab(23)(2))}
${numOrNearest(row(24), phraseRawTab(24)(2))}
${numOrNearest(row(25), phraseRawTab(25)(2))}
${numOrNearest(row(26), phraseRawTab(26)(2))}
${numOrNearest(row(27), phraseRawTab(27)(2))}
${numOrNearest(row(28), phraseRawTab(28)(2))}
${numOrNearest(row(29), phraseRawTab(29)(2))}
${numOrNearest(row(30), phraseRawTab(30)(2))}

== ${phraseRawTab(31)(2)} ==
${numOrNearest(row(31), phraseRawTab(32)(2))}
${numOrNearest(row(32), phraseRawTab(33)(2))}
${numOrNearest(row(33), phraseRawTab(34)(2))}
${numOrNearest(row(34), phraseRawTab(35)(2))}
${numOrNearest(row(35), phraseRawTab(36)(2))}
${numOrNearest(row(36), phraseRawTab(37)(2))}
${numOrNearest(row(37), phraseRawTab(38)(2))}

== ${phraseRawTab(39)(2)} ==
${supply(row(38), phraseRawTab(40)(2))}
${supply(row(39), phraseRawTab(41)(2))}
${supply(row(40), phraseRawTab(42)(2))}
${supply(row(41), phraseRawTab(43)(2))}
${supply(row(42), phraseRawTab(44)(2))}
${supply(row(43), phraseRawTab(45)(2))}
${supply(row(44), phraseRawTab(46)(2))}
${supply(row(45), phraseRawTab(47)(2))}

== ${phraseRawTab(48)(2)} ==
${available(row(46), phraseRawTab(49)(2))}
${available(row(47), phraseRawTab(50)(2))}
${available(row(48), phraseRawTab(51)(2))}
${available(row(49), phraseRawTab(52)(2))}

== ${phraseRawTab(53)(2)} ==
${availableOrNearest(row(50), phraseRawTab(54)(2))}
${availableOrNearest(row(51), phraseRawTab(55)(2))}
${availableOrNearest(row(52), phraseRawTab(56)(2))}
${availableOrNearest(row(53), phraseRawTab(57)(2))}
${availableOrNearest(row(54), phraseRawTab(58)(2))}
${availableOrNearest(row(55), phraseRawTab(59)(2))}
${availableOrNearest(row(56), phraseRawTab(60)(2))}
${availableOrNearest(row(57), phraseRawTab(61)(2))}
${availableOrNearest(row(58), phraseRawTab(62)(2))}
${availableOrNearest(row(59), phraseRawTab(63)(2))}
${availableOrNearest(row(60), phraseRawTab(64)(2))}
${availableOrNearest(row(61), phraseRawTab(65)(2))}
${availableOrNearest(row(62), phraseRawTab(66)(2))}
${availableOrNearest(row(63), phraseRawTab(67)(2))}
${availableOrNearest(row(64), phraseRawTab(68)(2))}
${availableOrNearest(row(65), phraseRawTab(69)(2))}
${availableOrNearest(row(66), phraseRawTab(70)(2))}
${connectedOrNearest(row(67), phraseRawTab(71)(2))}
${connectedOrNearest(row(68), phraseRawTab(72)(2))}
${connectedOrNearest(row(69), phraseRawTab(73)(2))}
${connectedOrNearest(row(70), phraseRawTab(74)(2))}
${hasOrNearest(row(71), phraseRawTab(75)(2))}
${hasOrNearest(row(72), phraseRawTab(76)(2))}
${hasOrNearest(row(73), phraseRawTab(77)(2))}
${hasOrNearest(row(74), phraseRawTab(78)(2))}
${hasOrNearest(row(75), phraseRawTab(79)(2))}

== ${phraseRawTab(80)(2)} ==
${availableOrNearest(row(76), phraseRawTab(81)(2))}
${availableOrNearest(row(77), phraseRawTab(82)(2))}
${availableOrNearest(row(78), phraseRawTab(83)(2))}
${availableOrNearest(row(79), phraseRawTab(84)(2))}
${availableOrNearest(row(80), phraseRawTab(85)(2))}
${availableOrNearest(row(81), phraseRawTab(86)(2))}
${availableOrNearest(row(82), phraseRawTab(87)(2))}
${availableOrNearest(row(83), phraseRawTab(88)(2))}

== ${phraseRawTab(89)(2)} ==
${availableOrNearest(row(84), phraseRawTab(90)(2))}
${availableOrNearest(row(85), phraseRawTab(91)(2))}
${availableOrNearest(row(86), phraseRawTab(92)(2))}
${availableOrNearest(row(87), phraseRawTab(93)(2))}
${availableOrNearest(row(88), phraseRawTab(94)(2))}
${availableOrNearest(row(89), phraseRawTab(95)(2))}
${availableOrNearest(row(90), phraseRawTab(96)(2))}
${availableOrNearest(row(91), phraseRawTab(97)(2))}
${availableOrNearest(row(92), phraseRawTab(98)(2))}
${availableOrNearest(row(93), phraseRawTab(99)(2))}
${availableOrNearest(row(94), phraseRawTab(100)(2))}
${availableOrNearest(row(95), phraseRawTab(101)(2))}
${availableOrNearest(row(96), phraseRawTab(102)(2))}

== ${phraseRawTab(103)(2)} ==
${availableOrNearest(row(97), phraseRawTab(104)(2))}
${availableOrNearest(row(98), phraseRawTab(105)(2))}
${availableOrNearest(row(99), phraseRawTab(106)(2))}
${availableOrNearest(row(100), phraseRawTab(107)(2))}

== ${phraseRawTab(108)(2)} ==
${row(1)} ${phraseRawTab(109)(2)}
${phraseRawTab(110)(2)}: ${row(103)}
${phraseRawTab(111)(2)}: ${row(104)}
${phraseRawTab(112)(2)}: ${row(105)}
${phraseRawTab(113)(2)}: ${row(106)}
${phraseRawTab(114)(2)}: ${row(107)}
${phraseRawTab(115)(2)}: ${row(108)}
${phraseRawTab(116)(2)}: ${row(109)}
${phraseRawTab(117)(2)}: ${row(110)}
${phraseRawTab(118)(2)}: ${row(111)}
${phraseRawTab(119)(2)}: ${row(112)}
${phraseRawTab(120)(2)}: ${row(113)}

== ${phraseRawTab(121)(2)} ==
${phraseRawTab(122)(2)}
${phraseRawTab(123)(2)}: ${row(114)}
${phraseRawTab(124)(2)}: ${row(115)}
${phraseRawTab(125)(2)}: ${row(116)}
${phraseRawTab(126)(2)}: ${row(117)}
${phraseRawTab(127)(2)}: ${row(118)}

== ${phraseRawTab(128)(2)} ==
${manuf(row)}
                  """

}
