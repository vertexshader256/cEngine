package tests.scala

import better.files.File
import tests.scala.TestClasses._

class ListTest extends StandardTest {
  "list test" should "print the correct results" in {

    val allCode = Seq(File("tests\\scala\\libds-master\\list.c").contentAsString,
      File("tests\\scala\\libds-master\\listtest.c").contentAsString)


    checkResults2(allCode)
  }
}

class VectorTest extends StandardTest {
  "vector test" should "print the correct results" in {

    val allCode =  Seq(File("tests\\scala\\libds-master\\vector.c").contentAsString,
                       File("tests\\scala\\libds-master\\vectest.c").contentAsString)


    checkResults2(allCode)
  }
}

class HeapTest extends StandardTest {
  "heap test" should "print the correct results" in {

    val allCode =  Seq(File("tests\\scala\\libds-master\\vector.c").contentAsString,
      File("tests\\scala\\libds-master\\heap.c").contentAsString,
      File("tests\\scala\\libds-master\\heaptest.c").contentAsString)


    checkResults2(allCode)
  }
}

class HashMapTest extends StandardTest {
  "heap test" should "print the correct results" in {

    val allCode =  Seq(File("tests\\scala\\libds-master\\vector.c").contentAsString,
      File("tests\\scala\\libds-master\\hashmap.c").contentAsString,
      File("tests\\scala\\libds-master\\maptest.c").contentAsString)


    checkResults2(allCode, false)
  }
}

//class AVLTreeTest extends StandardTest {
//  "avl test" should "print the correct results" in {
//
//    val allCode =  Seq(
//      File("tests\\scala\\libds-master\\avltree.c").contentAsString,
//      File("tests\\scala\\libds-master\\avl_example.c").contentAsString)
//
//
//    checkResults2(allCode, false)
//  }
//}
//
//class RegexTest extends StandardTest {
//  "regex test" should "print the correct results" in {
//
//    val allCode =  Seq(
//      File("tests\\scala\\libds-master\\slre.c").contentAsString,
//      File("tests\\scala\\libds-master\\unit_test.c").contentAsString)
//
//
//    checkResults2(allCode, false)
//  }
//}