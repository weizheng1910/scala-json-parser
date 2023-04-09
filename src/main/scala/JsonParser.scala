object JsonParser {
  val JSON_COMMA = ","
  val JSON_COLON = ":"
  val JSON_LEFTBRACKET = "["
  val JSON_RIGHTBRACKET = "]"
  val JSON_LEFTBRACE = "{"
  val JSON_RIGHTBRACE = "}"
  val JSON_QUOTE = "\""
  val JSON_WHITESPACE = Array(" ", "\t", "\b", "\n", "\r")
  val JSON_SYNTAX = Array(JSON_COMMA, JSON_COLON, JSON_LEFTBRACKET, JSON_RIGHTBRACKET,
  JSON_LEFTBRACE, JSON_RIGHTBRACE)

  // Entry point!
  def parser(jsonString: String): Map[Any,Any] = {
    parse(JsonLexer.lex(jsonString,List[Any]()))
  }

  def parse(array: List[Any]): Map[Any,Any] = {

    if (array.head == JSON_LEFTBRACE) {
      parseObject(array.tail)
    } else {
      throw new RuntimeException("JSON Object must start with left brace!")
    }

  }


  def parseKV(array: List[Any]): Tuple2[Option[Tuple2[Any,Any]],List[Any]] ={
    val (actualKey,rem) = parseKey(array)
    if(rem(0) != JSON_COLON) throw new RuntimeException("Key Value Exception: Key Value pair must have colon!")
    val (actualValue,rem1) = parseValue(rem.tail)
    (Some(actualKey.get,actualValue.get),rem1)
  }


  def parseElement(array: List[Any]): Tuple2[Option[Any], List[Any]] = {
    if(array(0) == JSON_LEFTBRACKET.toString){
      val (actualArray,rem) = parseArray(array.tail)
      (Some(actualArray),rem)
    } else {
      (Some(array(0)), array.tail)
    }
  }

  def parseKey(array: List[Any]): Tuple2[Option[Any], List[Any]] = {
    parseElement(array)
  }

  def parseValue(array: List[Any]): Tuple2[Option[Any], List[Any]] = {
    parseElement(array)
  }

  def parseArray(array: List[Any]): Tuple2[List[Any],List[Any]] = {

    val endIndex =  array.indexOf(JSON_RIGHTBRACKET)

    val elements: List[Any] = for {
      i <- List.range(0,endIndex)
      if(i % 2 == 0)
    } yield (array(i))

    val invalidElements = elements.find( e => JSON_SYNTAX contains e.toString)
    if(invalidElements.nonEmpty) throw new RuntimeException("Invalid JSON Syntax")

    (elements, array.drop(endIndex+1))
  }

  def parseObject(array: List[Any]): Map[Any,Any] ={

    var map: Map[Any,Any] = Map.empty

    if(array(1) == JSON_RIGHTBRACE){
      return map
    }

    var remaining:List[Any] = array

    while(remaining.size != 0 && remaining(0) != JSON_RIGHTBRACE){

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

   map
  }




}
