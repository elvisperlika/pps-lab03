package u03

object Streams extends App:

  import Sequences.*

  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])

  object Stream:

    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      // using reference by need:
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    extension [A](stream: Stream[A])
      def toList: Sequence[A] = stream match
        case Cons(h, t) => Sequence.Cons(h(), t().toList)
        case _ => Sequence.Nil()

      def map[B](f: A => B): Stream[B] = stream match
        case Cons(head, tail) => cons(f(head()), tail().map(f))
        case _ => Empty()

      def filter(pred: A => Boolean): Stream[A] = stream match
        case Cons(head, tail) if (pred(head())) => cons(head(), tail().filter(pred))
        case Cons(head, tail) => tail().filter(pred)
        case _ => Empty()

      def take(n: Int): Stream[A] = (stream, n) match
        case (Cons(head, tail), n) if n > 0 => cons(head(), tail().take(n - 1))
        case _ => Empty()
        

    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))



    // Task 3

    def takeWhile[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if pred(head()) => cons(head(), takeWhile(tail())(pred))
      case _ => Empty()
    
    def interleave[A](stream1: Stream[A], stream2: Stream[A]): Stream[A] = (stream1, stream2) match
      case (Cons(h1, t1), Cons(h2, t2)) => cons(h1(), cons(h2(), interleave(t1(), t2())))
      case (Cons(h1, t1), _) => cons(h1(), interleave(t1(), empty()))
      case (_, Cons(h2, t2)) => cons(h2(), interleave(empty(), t2()))
      case _ => empty()
      
    def fill[A](n: Int)(k: A): Stream[A] =
      take(iterate(k)(_ => k))(n)
      

  end Stream
end Streams

@main def tryStreams =
  import Streams.*

  val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
  val str2 = Stream.map(str1)(_ + 1) // {1,2,3,4,..}
  val str3 = Stream.filter(str2)(x => (x < 3 || x > 20)) // {1,2,21,22,..}
  val str4 = Stream.take(str3)(10) // {1,2,21,22,..,28}
  println(Stream.toList(str4)) // [1,2,21,22,..,28]

  lazy val corec: Stream[Int] = Stream.cons(1, corec) // {1,1,1,..}
  println(Stream.toList(Stream.take(corec)(10))) // [1,1,..,1]
