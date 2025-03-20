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
    def foldLeftInt(s: Sequence[Int])(acc: Int)(f: (Int, Int) => Int): Int = s match
        case Cons(h, t) => foldLeftInt(t)(f(acc, h))(f)
        case _ => acc

    @tailrec
    def foldLeft[A](s: Sequence[A])(acc: Int = 0)(f: (Int, A) => Int): Int = s match
        case Cons(h, t) => foldLeft(t)(f(acc, h))(f)
        case _ => acc

    def getNumberOfCourses(s: Sequence[Person]): Int =
        val teacher: Person => Boolean = _ match
            case TeacherPro(_, _) => true
            case Teacher(_, _) => true
            case _ => false

        val courses: Person => Sequence[String] = _ match
            case TeacherPro(_, c) => c
            case Teacher(_, c) => Cons(c, Nil())
            case _ => Nil()
      
        foldLeft(map(map(filter(s)(teacher))(courses))(_ => 1))(0)(_ + _)
        
