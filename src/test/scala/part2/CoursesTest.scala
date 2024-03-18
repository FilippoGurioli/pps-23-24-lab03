package task.part2

import u03.Sequences.*
import u02.Modules.Person
import u03.Sequences.Sequence.*
import org.junit.Test
import org.junit.Assert.*

class CoursesTest {

    val persons: Sequence[Person] = 
        Cons(Person.Teacher("Mirko Viroli", "PPS"), 
        Cons(Person.Student("Filippo Gurioli", 2024), 
        Cons(Person.Teacher("Danilo Pianini", "OOP"), Nil())))

    @Test def withAllKinds() = 
        assertEquals(Cons("PPS", Cons("OOP", Nil())), Sequences.courses(persons))
    
    @Test def withOnlyTeachers() =
        assertEquals(Cons("PPS", Cons("OOP", Nil())), Sequences.courses(filter(persons)(p => p match 
            case Person.Student(_, _) => false
            case Person.Teacher(_, _) => true
        )))
    
    @Test def withOnlyStudents() = 
        assertEquals(Nil(), Sequences.courses(filter(persons)(p => p match 
            case Person.Student(_, _) => true
            case Person.Teacher(_, _) => false
        )))

    @Test def withNil() = 
        assertEquals(Nil(), Sequences.courses(Nil()))
}
    