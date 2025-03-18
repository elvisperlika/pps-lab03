package u02

import org.junit.*
import org.junit.Assert.*

class Task2Test:

    import Modules.*
    import Modules.Person.*
    import u03.Sequences.*
    import u03.Sequences.Sequence.*
    import u02.Task2.*

    @Test def testFilterPersonGetOnlyTeachers(): Unit =
        val studentA = Student("sA", 2010)
        val studentB = Student("sB", 2011)
        val studentC = Student("sC", 2012)
        val teacherA = Teacher("tA", "math")
        val teacherB = Teacher("tB", "coding")
        val persons = Cons(studentA, Cons(teacherA, Cons(studentC, Cons(teacherB, Cons(studentC, Nil())))))
        import u02.Task2.*
        val courses = Cons("math", Cons("coding", Nil()))

        assertEquals(courses, getTeacherCourse(persons))

    @Test def testFoldLeft(): Unit =
        val list = Cons(1, Cons(2, Cons(5, Cons (2, Nil()))))
        val sum: (Int, Int) => Int = _ + _
        assertEquals(10, foldLeft(list)(0)(sum))
        assertEquals(10, foldLeftGeneric(list)(0)(sum))
        val dif: (Int, Int) => Int = _ - _
        assertEquals(-10, foldLeft(list)(0)(dif))
        assertEquals(-10, foldLeftGeneric(list)(0)(dif))

