import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Try, Success, Failure}

def summingPairs(xs: Vector[Int], sum: Int): Vector[Tuple2[Int,Int]] = {
  def summingPairsHelper(xs: Vector[Int],
                         the_pairs: Vector[Tuple2[Int,Int]]): Vector[Tuple2[Int,Int]] =
    xs match {
      case fst +: rest =>
      val first=rest.slice (0, rest.length/2)
      val second=rest.slice(rest.length/2, rest.length-1)
        val pairs1: Future[(Int, Int)] = first.collect({case snd if fst + snd <= sum => Future{(fst,snd)}})
        val pairs2: Future[(Int, Int)] = second.collect({case snd if fst + snd <= sum => Future{(fst,snd)}})

        summingPairsHelper(rest, the_pairs ++ pairs1 ++ pairs2)
      case _ => the_pairs // If there's no head element, the vector is empty.
    }
  
  summingPairsHelper(xs,Vector())
}