package com.cmnguyen.pattern.applicative

import java.util.Date
import java.text._

object ApplicativeExample {

  sealed trait Validation[+E, +A]

  case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]

  case class Success[A](a: A) extends Validation[Nothing, A]

  case class WebForm(name: String, birthday: Date, phoneNumber: String)

  def validName(name: String): Validation[String, String] =
    if (name != "") Success(name)
    else Failure("Name can not be empty")

  def validBirthday(birthday: String): Validation[String, Date] =
    try {
      Success(new SimpleDateFormat("yyyy-MM-dd").parse(birthday))
    } catch {
      case _ => Failure("Birthday must be in the form yyyy-MM-dd")
    }

  def validPhone(phoneNumber: String): Validation[String, String] =
    if (phoneNumber.matches("[0-9]{10}")) Success(phoneNumber)
    else Failure("phone number must contain 10 digits")

  def validationApplicative[E]: Applicative[({type f[x] = ApplicativeExample.this.Validation[E, x]})#f] =
    new Applicative[({ type f[x] = Validation[E, x] })#f] {

      override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] =
        (fa, fb) match {
          case (Failure(h1, t1), Failure(h2, t2)) => Failure[E](h1, t1++Vector(h2)++t2)
          case (Success(a), Success(b)) => Success(f(a, b))
          case (Failure(h1, t1), _) => Failure(h1, t1)
          case (_, Failure(h2, t2)) => Failure(h2, t2)
        }

      override def unit[A](a: => A): Validation[E, A] = Success(a)
    }

  def validWebForm(name: String,
                   birthday: String,
                   phoneNumber: String): Validation[String, WebForm] =
    validationApplicative.map3(validName(name), validBirthday(birthday), validPhone(phoneNumber))(WebForm)

  def main(args: Array[String]): Unit = {
    val validation = validWebForm("", "2000-12-1", "012345678")
    println(s"isWebFormValid $validation")
  }

}
