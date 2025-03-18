package u02

import u02.Modules.Person

import scala.annotation.tailrec

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

    @tailrec
    def foldLeft(s: Sequence[Int])(acc: Int)(f: (Int, Int) => Int): Int = s match
        case Cons(h, t) => foldLeft(t)(f(acc, h))(f)
        case _ => acc
        
    @tailrec
    def foldLeftGeneric[A](s: Sequence[A])(acc: A)(f: (A, A) => A): A = s match
        case Cons(h, t) => foldLeftGeneric(t)(f(acc, h))(f)
        case _ => acc