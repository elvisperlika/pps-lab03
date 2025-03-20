package u02

import org.junit.*
import org.junit.Assert.*
import u03.Streams

class Task2Test:

    import Modules.*
    import Modules.Person.*
    import u03.Sequences.*
    import u03.Sequences.Sequence.*
    import u02.Task2.*

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

    @Test def testFoldLeft(): Unit =
        val list = Cons(1, Cons(2, Cons(5, Cons (2, Nil()))))
        val sum: (Int, Int) => Int = _ + _
        assertEquals(10, foldLeftInt(list)(0)(sum))
        assertEquals(10, foldLeft(list)(0)(sum))
        val dif: (Int, Int) => Int = _ - _
        assertEquals(-10, foldLeftInt(list)(0)(dif))
        assertEquals(-10, foldLeft(list)(0)(dif))


    @Test def testGetNumberOfCourses(): Unit =
        val viroli = Teacher("Viroli", "pps")
        val ricci = Teacher("ricci", "pcd")
        val angelo = Teacher("angelo", "cs")
        val bob = Student("bob", 2011)
        val persons = Cons(bob, Cons(viroli, Cons(ricci, Cons(angelo, Nil()))))
        assertEquals(3, getNumberOfCourses(persons))

    @Test def testFill(): Unit =
        import Streams.*
        val s3 = Stream.fill(3)("s")
        val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
        assertEquals(Stream.toList(s3), Cons("s", Cons("s", Cons("s", Nil()))))