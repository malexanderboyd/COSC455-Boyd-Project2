def isPrime(num : Int) : Boolean = {
  if(num == 1) false
  else
  (2 until num) forall (num % _ != 0)
}

def isTwinPrime(num1 : Int, num2: Int) : Boolean = {
  if(isPrime(num1) && isPrime(num2))
    Math.abs(num1 - num2) == 2
  else
    false
}
def twinPrimeList2(intList : Int) : String = {
  var resolvedList = Array[Int]()
  var output : String = "null"
  for( a <- 2 to intList){
    if(isPrime(a))
    {
      var b = a + 2
      if(isTwinPrime(a, b)) {
        if (!resolvedList.contains(a))
          resolvedList = resolvedList :+ a
        if (!resolvedList.contains(b))
          resolvedList = resolvedList :+ b
      }
    }
  }
  if(resolvedList.length > 0) {
    resolvedList.deep.mkString(" ")
  }
  else
    output
}
var output = List[String]()
def twinPrimeList(input : Int) : List[String] = {
  var curr : Int = input
  curr match {
    case 1 => // prime must be >1
    case 2 => //2 is a prime but not a twin prime (2+2 = 4 =/= Prime)
    case _ =>
      if(isPrime(input))
      {
      if(isTwinPrime(input, input-2)) {
        if(!output.contains(input.toString))
          output = input.toString :: output
          output = (input-2).toString :: output
        curr = curr - 1
        output = twinPrimeList(curr)
      }
        else
        {
          curr = curr - 1
          output = twinPrimeList(curr)
        }
      } else
     {
       curr = curr - 1
      output = twinPrimeList(curr)
    }
    }
    output
}

var tacos : List[String] = twinPrimeList(50)


def goldbach ( goldMember : Int) : String = {
  if(goldMember > 2 && goldMember % 2 == 0)
  {
    "Goldbach Number: " + goldMember + "\n" + solveGoldBach(goldMember)
  }
  else
  {
    "Error: number must be even and above 2."
  }
}
var i : Int = 2
var output3 : String = ""
def solveGoldBach(g : Int) : String = {
  if(i < g/2 ) {
    var j = g - i
    isPrime(i) && isPrime(j) match {
      case true => output3 = output3.concat(" " + i.toString + " + " + j.toString)
        i = i + 1
        solveGoldBach(g)
      case false => i = i + 1
        solveGoldBach(g)
    }
  }
  else
    output3
}

goldbach(28)