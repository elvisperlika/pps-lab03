package u02

import org.junit.*
import org.junit.Assert.*

class Task2Test:

    import Modules.*
    import Modules.Person.*
    import u03.Sequences.*
    import u03.Sequences.Sequence.*

    val studentA = Student("sA", 2010)
    val studentB = Student("sB", 2011)
    val studentC = Student("sC", 2012)
    val teacherA = Teacher("tA", "math")
    val teacherB = Teacher("tB", "coding")

    val persons = Cons(studentA, Cons(teacherA, Cons(studentC, Cons(teacherB, Cons(studentC, Nil())))))
    @Test def testFilterPersonGetOnlyTeachers(): Unit =
        import u02.Task2.*
        val courses = Cons("math", Cons("coding", Nil()))
        assertEquals(courses, getTeacherCourse(persons))
