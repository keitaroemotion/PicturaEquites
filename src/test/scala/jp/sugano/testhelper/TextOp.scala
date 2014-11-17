package jp.sugano.testhelper

trait TextOp {

  def extendMap(map:Map[String, List[String]], alpha:(String, String)):Map[String, List[String]] = {
    alpha._1 match {
      case _ if map.contains(alpha._1) => map + (alpha._1 -> (map(alpha._1) :+  alpha._2 ))
      case _ => map + (alpha._1 -> List(alpha._2))
    }
  }

  def parseConfigLines(lines:List[String]):Map[String, List[String]] = {
    lines.foldLeft(Map[String, List[String]]()) { (map, line) =>
      extendMap(map, convertLineToKeyValuePair(line))
    }
  }


  def convertLineToKeyValuePair(line:String):(String, String) = {
    var flag = false
    var tmp = ""
    var attr_key = ""
    var tail = ""

    line.toCharArray.foreach{ c =>
      c match {
        case ' ' if flag == false => {
          flag = true
          attr_key = tmp
          tmp = ""
        }
        case _ => tmp += c
      }
    }
    tail = tmp
    (attr_key, tmp)
  }
}
