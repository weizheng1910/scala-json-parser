import exception.InvalidSyntaxException

import scala.collection.mutable.ListBuffer

object JsonParser {
  val JSON_COMMA = ","
  val JSON_COLON = ":"
  val JSON_LEFTBRACKET = "["
  val JSON_RIGHTBRACKET = "]"
  val JSON_LEFTBRACE = "{"
  val JSON_RIGHTBRACE = "}"
  val JSON_QUOTE = "\""
  val JSON_WHITESPACE: Array[String] = Array(" ", "\t", "\b", "\n", "\r")
  val JSON_SYNTAX: Array[String] = Array(JSON_COMMA, JSON_COLON, JSON_LEFTBRACKET, JSON_RIGHTBRACKET,
  JSON_LEFTBRACE, JSON_RIGHTBRACE)


  def parser(jsonString: String): Object = {
    parse(JsonLexer.lex(ListBuffer[Any](),jsonString))._1
  }

  def parse(array: List[Any]): (Object,List[Any]) = {

    if (array.head == JSON_LEFTBRACE) {
      parseObject(array.tail)
    } else if(array.head == JSON_LEFTBRACKET){
      parseArray(array.tail)
    } else {
      throw new InvalidSyntaxException("Invalid starting character")
    }
  }


  def parseKV(list: List[Any]): (Option[(Any,Any)],List[Any]) ={
    val (actualKey,rem) = parseKey(list)
    if(rem.head != JSON_COLON) throw new InvalidSyntaxException("Expect a ':' but actually get a " + rem.head)
    val (actualValue,rem1) = parseValue(rem.tail)
    (Some(actualKey.get,actualValue.get),rem1)
  }


  def parseElement(list: List[Any]): (Option[Any], List[Any]) = {
    if(list.head == JSON_LEFTBRACKET){
      val (actualArray,rem) = parseArray(list.tail)
      (Some(actualArray),rem)
    } else {
      (Some(list.head), list.tail)
    }
  }

  def parseKey(list: List[Any]): (Option[Any], List[Any]) = {
    parseElement(list)
  }

  def parseValue(list: List[Any]): (Option[Any], List[Any]) = {
    parseElement(list)
  }

  def parseArray(list: List[Any]): (List[Any],List[Any]) = {
    var remaining = list
    val result = ListBuffer[Any]()
    while(remaining.nonEmpty && remaining.head != JSON_RIGHTBRACKET){
      if((JSON_WHITESPACE contains remaining.head.toString)
        || JSON_COMMA == remaining.head.toString){
        remaining = remaining.tail
      } else if(remaining.head == JSON_LEFTBRACE){
        val res = parseObject(remaining.tail)
        result += res._1
        remaining = res._2
      } else {
        result += remaining.head
        remaining = remaining.tail
      }
    }

    if(remaining.head == JSON_RIGHTBRACKET){
     remaining = remaining.tail
    }

    (result.result(), remaining)
  }

  def parseObject(list: List[Any]): (Map[Any,Any], List[Any]) ={

    var map: Map[Any,Any] = Map.empty

    if(list.head == JSON_RIGHTBRACE){
      return (map,list.tail)
    }

    var remaining:List[Any] = list

    while(remaining.nonEmpty && remaining.head != JSON_RIGHTBRACE){

      if((JSON_WHITESPACE contains remaining.head.toString)
        || JSON_COMMA == remaining.head.toString){
        remaining = remaining.tail
      }

      val (res,rem) = parseKV(remaining)

      if(res.nonEmpty){
        map += res.get
      }

      remaining = rem
    }

    (map,remaining.tail)
  }




}
