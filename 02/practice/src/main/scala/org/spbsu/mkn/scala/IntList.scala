package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.IntList._

sealed trait IntList {

  def head: Int
  def tail: IntList

  def drop(n: Int): IntList

  def take(n: Int): IntList
  def map(f: Int => Int): IntList
  def ::(elem: Int): IntList = {
    Cons(elem, this)
  }
}

case object IntNil extends IntList {
  def head: Int = {
    undef
  }
  def tail: IntList =  {
    undef
  }
  override def drop(n: Int): IntList = {
    n match {
      case 0 => IntNil
      case _ => undef
    }
  }
  override def take(n: Int): IntList = {
    n match {
      case 0 => IntNil
      case _ => undef
    }
  }
  def map(f: Int => Int): IntList = {
    IntNil
  }
}

case class Cons(head_ : Int, tail_ : IntList) extends IntList {
  def head: Int = {
    head_
  }
  def tail: IntList =  {
    tail_
  }
  override def drop(n: Int): IntList = {
    n match {
      case 0 => this
      case n => tail_.drop(n - 1)
    }
  }
  override def take(n: Int): IntList = {
    n match {
      case 0 => IntNil
      case n => head_ :: tail_.take(n - 1)
    }
  }
  def map(f: Int => Int): IntList = {
    Cons(f(head_), tail_.map(f))
  }
}


object IntList {
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")
  def fromSeq(seq: Seq[Int]): IntList = {
    seq.foldRight(IntNil: IntList)((elem, list) => elem :: list)
  }

  def sum(intList: IntList): Int      = {
    intList match {
      case Cons(head_, IntNil) => head_
      case Cons(head_, tail_) => head_ + sum(tail_)
      case IntNil => undef
    }
  }
  def size(intList: IntList): Int     = {
    intList match {
      case Cons(_, tail_) => 1 + size(tail_)
      case IntNil => 0
    }
  }
  // extra task: implement sum using foldLeft
  // def foldLeft(???)(???): ??? = ???
}

