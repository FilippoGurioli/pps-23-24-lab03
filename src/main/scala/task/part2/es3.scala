package task.part2

import u03.Sequences.*
import u02.Modules.Person
import u03.Sequences.Sequence.*

object Sequences:
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