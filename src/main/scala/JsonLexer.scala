import JsonParser.{JSON_QUOTE, JSON_SYNTAX, JSON_WHITESPACE}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

object JsonLexer {

  @tailrec
  def lex( array: ListBuffer[Any],string: String): List[Any] = {

    val (a,s) = lexAll((array,string))

    if(s.isEmpty){
      return a.result()
    }

    lex(a,s)
  }

  val lexAll: ((ListBuffer[Any], String)) => (ListBuffer[Any], String) = lexBoolean _ andThen lexString andThen lexInteger

  def lexAny(tuple: (ListBuffer[Any], String)): (ListBuffer[Any], String) = {

    var elements = tuple._1
    var remaining = tuple._2

    while((JSON_WHITESPACE contains remaining.take(1)) || (JSON_SYNTAX contains remaining.take(1))){
      if(JSON_SYNTAX contains remaining.take(1)){
       elements += remaining.take(1)
      }

      remaining = remaining.substring(1)
    }

    (elements,remaining)
  }

  def lexString(tuple: (ListBuffer[Any], String)): (ListBuffer[Any], String) = {

    val res = lexAny(tuple)
    val array = res._1
    val str = res._2

    if(str.isEmpty){
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

  def lexInteger(tuple: (ListBuffer[Any], String)): (ListBuffer[Any], String) = {

    val res = lexAny(tuple)
    val array = res._1
    val str = res._2

    if(str.isEmpty){
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

    (array += arrSplit._1, arrSplit._2)
  }

  def lexBoolean(tuple: (ListBuffer[Any], String)): (ListBuffer[Any], String) = {

    val res = lexAny(tuple)
    val array = res._1
    val str = res._2

    val indexTrue = str.indexOf("true")
    val indexFalse = str.indexOf("false")

    if(indexTrue == 0){
      (array += true, str.substring("true".length))
    } else if(indexFalse == 0){
      (array += false, str.substring("false".length))
    } else {
      (array, str)
    }

  }


}
