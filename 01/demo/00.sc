import scala.collection.immutable.Stream.Empty.print
//todo: What is Scala?

//define method isPrime

//print all primes from 1 to 100
val x = 12323

var y = "kfk"
y = "13"
val res0 = 1
def is_prime(n: Int): Boolean = n > 1 && (2 until n).forall(i => n % i != 0)
is_prime(13)

for {
  i <- 1 to 100
  if is_prime(i)
} print(s"$i ")
