package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.MyGenericList._

sealed trait MyGenericList[+T] {
  def head: T
  def tail: MyGenericList[T]
  def drop(n: Int): MyGenericList[T]
  def take(n: Int): MyGenericList[T]
  def map[NT](f: T => NT): MyGenericList[NT]
  def ::[NT >: T](elem: NT): MyGenericList[NT] = {
    Cons[NT](elem, this)
  }
}

object MyGenericList {
  def empty[T]() : MyGenericList[T] = {
    MyNil
  }
  def undef:  Nothing = throw new UnsupportedOperationException("operation is undefined")
  def fromSeq[T](seq: Seq[T]): MyGenericList[T] = {
    seq.foldRight(MyGenericList.empty[T]())((elem : T , list: MyGenericList[T]) => elem :: list)
  }

  def size[T](list: MyGenericList[T]): Int = {
    list match {
      case Cons(_, tail_) => 1 + size(tail_)
      case MyNil => 0
    }
  }

  def sum[T <: Int](intList: MyGenericList[T]): Int = {
    intList match {
      case Cons(head_, MyNil) => head_
      case Cons(head_, tail_) => head_ + sum(tail_)
      case MyNil => undef
    }
  }
  // extra task: implement sum using foldLeft
  // def foldLeft(???)(???): ??? = ???
}


case object MyNil extends MyGenericList[Nothing] {
  def head: Nothing = {
    undef
  }
  def tail: MyGenericList[Nothing] = {
    undef
  }
  override def drop(n: Int): MyGenericList[Nothing] = {
    n match {
      case 0 => MyNil
      case _ => undef
    }
  }
  override def take(n: Int): MyGenericList[Nothing] = {
    n match {
      case 0 => MyNil
      case _ => undef
    }
  }
  def map[NT](f: Nothing => NT): MyGenericList[NT] = {
    MyNil
  }
}

case class Cons[T](head : T, tail : MyGenericList[T]) extends MyGenericList[T] {
  override def drop(n: Int): MyGenericList[T] = {
    n match {
      case 0 => this
      case n => tail.drop(n - 1)
    }
  }
  override def take(n: Int): MyGenericList[T] = {
    n match {
      case 0 => MyNil
      case n => head :: tail.take(n - 1)
    }
  }
  def map[NT](f: T => NT): MyGenericList[NT] = Cons[NT](f(head), tail.map[NT](f))

}



