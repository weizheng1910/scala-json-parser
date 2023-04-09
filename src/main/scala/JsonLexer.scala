import JsonParser.{JSON_QUOTE, JSON_SYNTAX, JSON_WHITESPACE}
import scala.annotation.tailrec
import scala.util.matching.Regex

object JsonLexer {

  @tailrec
  def lex(string: String, array: List[Any]): List[Any] = {

    val (a,s) = lexAll((array,string))

    if(s.length == 0){
      return a
    }

    lex(s,a)
  }

  val lexAll = lexBoolean _ andThen lexString _ andThen lexInteger _

  def lexAny(tuple: (List[Any], String)): (List[Any], String) = {

    var a = tuple._1
    var s = tuple._2

    while((JSON_WHITESPACE contains s.take(1)) || (JSON_SYNTAX contains s.take(1))){
      if(JSON_SYNTAX contains s.take(1)){
        a = a :+ s.take(1)
      }

      s = s.substring(1)
    }

    (a,s)
  }

  def lexString(tuple: (List[Any], String)): (List[Any], String) = {

    val res = lexAny(tuple)
    val array = res._1
    val str = res._2

    if(str.length == 0){
      return (array ,str)
    }

    if(str.substring(0,1) != JSON_QUOTE){
      return (array, str)
    }

    var string = str.substring(1)

    if(!string.contains(JSON_QUOTE)) {
      throw new RuntimeException("Invalid String")
    }

    val splitArr = string.span(p => p.toString != JSON_QUOTE)

    (array :+ splitArr._1, splitArr._2.substring(1))
  }

  def lexInteger(tuple: (List[Any], String)): (List[Any], String) = {

    val res = lexAny(tuple)
    val array = res._1
    val str = res._2

    if(str.length == 0){
      return (array ,str)
    }

    val numberPattern: Regex = "[0-9]+".r

    val endIndex = numberPattern.findFirstMatchIn(str) match {
      case Some(substr) => substr.end
      case None => -1
    }

    if(endIndex == -1){
      return (array, str)
    }

    val arrSplit = str.splitAt(endIndex)

    ((array :+ arrSplit._1), arrSplit._2)
  }

  def lexBoolean(tuple: (List[Any], String)): (List[Any], String) = {

    val res = lexAny(tuple)
    val array = res._1
    val str = res._2

    val indexTrue = str.indexOf("true")
    val indexFalse = str.indexOf("false")

    if(indexTrue == 0){
      (array :+ true, str.substring("true".length))
    } else if(indexFalse == 0){
      (array :+ false, str.substring("false".length))
    } else {
      (array, str)
    }

  }

}
