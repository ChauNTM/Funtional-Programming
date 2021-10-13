package com.cmnguyen

import java.io.File

import com.cmnguyen.io.IO
import com.cmnguyen.process.{Await, Emit, Halt, Process}

object Main {

  def main(args: Array[String]): Unit = {

    //    val p = Process.lift((x: Int) => x + 1)
    //    val xs = p(LazyList(1, 2, 3, 4, 5)).toList
    //    println(s"result $xs")
    //
    //    val even = Process.filter((x: Int) => x % 2 == 0)
    //    val evenList= even(LazyList(2, 11, 5, 7, 4)).toList
    //    println(s"even $evenList")
    //
    //    val sum = Process.sum
    //    val sumValue = sum(LazyList(2, 11, 5, 7, 4)).toList
    //    println(s"sum $sumValue")
    //
    //    val liftOne = Process.liftOne((x: Int) => x + 1)
    //    val plus = liftOne ++ even
    //    val plusValue= plus(LazyList(2, 4, 5, 8)).toList
    //    println(s"plus $plusValue")
    //
    //    val flatMap = even.flatMap(i => p)
    //    val flatMapValue = flatMap(LazyList(2, 4, 5, 8)).toList
    //    println(s"flatMap $flatMapValue")
    //
    //    val zipWithIndexValue = even.zipWithIndex(LazyList(2, 4, 5, 8)).toList
    //    println(s"zipWithIndex $zipWithIndexValue")

    /**
     * filePath is the path of the file need to be checked
     * n is the number of lines
     * result return whether the file contains more than n lines
     */
//    val file = new File("filePath")
//    val n = 51
//    val result = processFile(file, Process.count1[String] |> Process.exists(_ > n), false)(_ || _).run



  }

  def processFile[A, B](f: File,
                        p: Process[String, A],
                        z: B)(g: (B, A) => B): IO[B] = IO {
    def go(ss: Iterator[String], cur: Process[String, A], acc: B): B =
      cur match {
        case Halt() => acc
        case Emit(head, tail) => go(ss, tail, g(acc, head))
        case Await(recv) =>
          val next = if (ss.hasNext) recv(Some(ss.next())) else recv(None)
          go(ss, next, acc)
      }

    val s = scala.io.Source.fromFile(f)
    try go(s.getLines(), p, z)
    finally s.close()
  }

}
