package u03

import u02.AnonymousFunctions.l
import u03.Optionals.Optional

object Sequences: // Essentially, generic linkedlists
  
  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _          => 0

    // def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
    //   case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
    //   case Nil()      => Nil()
    
    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = 
      flatMap[A, B](l)(e => Cons(mapper(e), Nil()))
    

    // def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
    //   case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
    //   case Cons(_, t)            => filter(t)(pred)
    //   case Nil()                 => Nil()
    
    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = 
      flatMap[A, A](l1)(e => e match { case e if pred(e) => Cons(e, Nil()) case _ => Nil() })

    // Lab 03
    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
      case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), zip(t1, t2))
      case _ => Nil()
    

    def take[A](l: Sequence[A])(n: Int): Sequence[A] = l match
      case Cons(h, t) if n > 0 => Cons(h, take(t)(n - 1))
      case _ => Nil()
    
    
    def concat[A](l1: Sequence[A], l2: Sequence[A]): Sequence[A] = (l1, l2) match
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1, concat(t1, Cons(h2, t2)))
      case (Nil(), Cons(h2, t2)) => l2
      case (Cons(h1, t1), Nil()) => l1
      case _ => Nil()      
    
    def flatMap[A, B](l: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = l match
      case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))
      case _ => Nil()
    

    def min(l: Sequence[Int]): Optional[Int] = l match
      case Cons(h, t) => (h,t) match 
        case (head, Cons(h, t)) => min(
          head match
            case head if head >= h => Cons(h, t)
            case _ => Cons(head, t)
        )
        case _ => Optional.Just(h)
      case _ => Optional.Empty()
  
    import u02.Modules.Person 
    
    def courses(persons: Sequence[Person]): Sequence[String] = filter[String](
          map[Person, String](persons)(person => person match
              case Person.Teacher(_, courses) => courses
              case _ => ""
          )
      )(s => s != "")

    def foldLeft[E](seq: Sequence[E])(defaultValue: E)(accumulator: (E, E) => E): E = seq match
        case Nil() => defaultValue
        case Cons(h, t) => foldLeft[E](t)(accumulator(defaultValue, h))(accumulator)
    
    extension (persons: Sequence[Person]) def coursesWithExtension(): Sequence[String] = Sequences.courses(persons)
    
    extension [E](seq: Sequence[E]) def foldLeftWithExtension(defaultValue: E)(accumulator: (E, E) => E): E = foldLeft(seq)(defaultValue)(accumulator)

object Streams:

  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])

  object Stream:
    import Sequences.*

    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    def toList[A](stream: Stream[A]): Sequence[A] = stream match
      case Cons(h, t) => Sequence.Cons(h(), toList(t()))
      case _ => Sequence.Nil()

    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match
      case Cons(head, tail) => cons(f(head()), map(tail())(f))
      case _ => Empty()

    def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if (pred(head())) => cons(head(), filter(tail())(pred))
      case Cons(head, tail) => filter(tail())(pred)
      case _ => Empty()

    def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(head, tail), n) if n > 0 => cons(head(), take(tail())(n - 1))
      case _ => Empty()

    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))

    // Task 3

    def takeWhile[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if pred(head()) => cons(head(), takeWhile(tail())(pred))
      case _ => Empty()
    
    def fill[A](n: Int)(value: A): Stream[A] = n match
      case n if n > 0 => Cons(() => value, () => fill(n - 1)(value))
      case _ => Empty()

  end Stream