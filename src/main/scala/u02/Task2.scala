package u02

import u02.Modules.Person

object Task2:

  import u02.Modules.Person.*
  import u03.Sequences.*
  import u03.Sequences.Sequence.*

    def getTeacherCourse(s: Sequence[Person]): Sequence[String] =
        val teacherPredicate: Person => Boolean = {
            case Teacher(_, _) => true
            case _ => false
        }
        val getCourseFromTeacher: Person => String = {
            case Teacher(_, c) => c
            case _ => ""
        }

        map(filter(s)(teacherPredicate))(getCourseFromTeacher)


