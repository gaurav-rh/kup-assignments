package edu.knoldus

class Searching extends App{

  def binarySearch(array: Array[Int], elem: Int): Boolean = {
    val arraySort = array.sortBy(a => a)
    if (arraySort.length > 1) {
      if (elem < arraySort(arraySort.length / 2)) binarySearch(arraySort.take((arraySort.length / 2)), elem)
      else binarySearch(arraySort.drop(arraySort.length / 2), elem)
    }
    else if ((arraySort.length == 1) && arraySort(0) == elem) true
    else false
  }

  def linearSearch(array: Array[Int], elem: Int): Boolean ={
  def linearSearcherHelper(index: Int): Boolean =
    if (elem == array(index)) true
    else if (index < array.length - 1) linearSearcherHelper(index + 1)
    else false
  linearSearcherHelper(0)
}
}
