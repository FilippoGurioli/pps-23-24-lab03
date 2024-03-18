package task.part2

import u03.Sequences.*
import u02.Modules.Person
import u03.Sequences.Sequence.*

object Sequences:
    def courses: Sequence[Person] => Sequence[String] = persons => filter[String](
        map[Person, String](persons)(person => person match
            case Person.Teacher(_, courses) => courses
            case _ => ""
        )
    )(s => s != "")
    