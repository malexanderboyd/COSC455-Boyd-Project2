import scala.collection.mutable

val chinese: List[String] = List("ling", "yi", "er", "san", "si", "wu", "liu", "qi", "ba", "jiu", "shi")
val english: List[String] = List("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")
val allLang: List[String] = english ::: chinese
val mapping = new mutable.HashMap[String, String]()

def initTranslationMap() = {
  for(i <- 0 to english.size-1)
    mapping.put(chinese(i), english(i))
}

var output : List[Int] = List()
var printedHeaders : Boolean = false
def go(c : List[String]) : List[Int] = {
  initTranslationMap()
  var filterResult = c.filter(allLang.contains(_))
  filterResult match {
    case Nil => Nil
    case head :: tail =>
              if(chinese contains head)
                output = english2Num(chinese2English(head)) :: output
              else
                output = english2Num(head) :: output
              go(tail)
              printedHeaders = true
  }
  if(!printedHeaders) {
    println("Translation: " + output.reverse.mkString(" "))
    println("Addition: " + output.reverse.mkString(" + ") + " = " + output.sum)
    println("Multiplication : " + output.reverse.mkString(" * ") + " = " + output.product)
  }
  output
}

def english2Num (engWord : String ) : Int = {
  engWord match {
  case "zero" => 0
  case "one" => 1
  case "two" => 2
  case "three" => 3
  case "four" => 4
  case "five" => 5
  case "six" => 6
  case "seven" => 7
  case "eight" => 8
  case "nine"   => 9
  case "ten" => 10
  }
}

def chinese2English (chineseWord : String) : String = {
  mapping(chineseWord)
}


go(List("yi","nine", "six", "ba", "taco"))