package part2

import u03.Sequences.Sequence.*
import u03.Sequences.*
import org.junit.Test
import org.junit.Assert.*
import task.part2.Sequences.foldLeft
import task.part2.Sequences.foldLeftWithExtension

class FoldLeftTest {

    val seq: Sequence[Int] = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))

    @Test def differenceTest() = 
        assertEquals(-16, foldLeft(seq)(0)(_ - _))

    @Test def sumTest() =
        assertEquals(16, foldLeft(seq)(0)(_ + _))

    @Test def empty() =
        assertEquals(5, foldLeft(Nil())(5)(_ + _))

    @Test def usingExtension() =
        assertEquals(16, seq.foldLeftWithExtension(0)(_ + _))
    
    // @Test def differentOutput() = 
    //     assertEquals(???, foldLeft(Nil())(5)((a,b) => println(a + " " + b)))
}
