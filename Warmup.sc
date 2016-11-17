
def isPrime(num : Int) : Boolean = {
  (2 until num) forall (num % _ != 0)
}

def isTwinPrime(num1 : Int, num2: Int) : Boolean = {
  if(isPrime(num1) && isPrime(num2))
    Math.abs(num1 - num2) == 2
  else
    false
}
def twinPrimeList(intList : Int) : String = {
  var resolvedList = Array[Int]()
  for( a <- 2 to intList){
    if(isPrime(a))
      {
        var b = a + 2
        if(isTwinPrime(a, b))
          resolvedList = resolvedList :+ a
      }
  }
  if(resolvedList.length > 0)
    resolvedList.mkString
  else
    Array(1,3,3,7).mkString
}

twinPrimeList(50)