package edu.knoldus

import scala.annotation.tailrec

class Sorting {

  def insertionSort(array: Array[Int]): Array[Int] = {
    val list: List[Int] = array.toList
    def helperInsertionSort(list: List[Int]): List[Int] = {
      def insertionInner(x: Int, list: List[Int]): List[Int] = {
        if (list.isEmpty || x <= list.head) {
          x :: list
        } else {
          list.head :: insertionInner(x, list.tail)
        }
      }
      if (list.isEmpty) {
        Nil
      } else {
        insertionInner(list.head, helperInsertionSort(list.tail))
      }
    }
    helperInsertionSort(list).toArray
  }

  def selectionSort(array: Array[Int]): Array[Int] = {
    val list = array.toList
    @tailrec
    def selectionSortHelper(list1:List[Int], accumlist:List[Int], index:Int):List[Int] = {
      if (index < list1.length) {
        val min=list1.min
        if(min == list1(index)) selectionSortHelper(list1.take(index) ::: list1.drop(index+1), accumlist :+ list1(index), 0)
        else selectionSortHelper(list1, accumlist, index + 1)
      }
      else {
        if(list1.nonEmpty) selectionSortHelper(list1, accumlist, 0)
        else accumlist
      }
    }

    selectionSortHelper(list,List[Int](),0).toArray
  }

  def bubbleSort(array: Array[Int]): Array[Int] = {
    val list = array.toList
    def swapList(list:List[Int],index1:Int,index2:Int):List[Int] ={
      val newList:List[Int] = list.dropRight(list.length - index1) ::: (list(index2) :: list.dropRight(list.length - index2).drop(index1 + 1)) ::: list(index1) :: list.drop(index2 + 1)
      newList
    }
    @tailrec
    def bubbleSortJunior(list: List[Int], pos: Int): List[Int] = {
      if (pos >= 0) bubbleSortJunior( bubbleSortHelper(list, 0),pos-1)
      else list
    }
    @tailrec
    def bubbleSortHelper(list: List[Int], index: Int): List[Int] = {
      if (index + 1 < list.length){
        if (list(index) > list(index + 1)) bubbleSortHelper(swapList(list, index, index + 1), index+1)
        else bubbleSortHelper(list, index + 1)
      }
      else list
    }
    val pos =list.length-1
    bubbleSortJunior(list, pos).toArray
  }

}
