package scala.c.engine

import better.files.File

class ListTest extends StandardTest {
  "list test" should "print the correct results" in {

    val allCode = Seq(File("tests\\scala\\c\\engine\\libds-master\\list.c").contentAsString,
      File("tests\\scala\\c\\engine\\libds-master\\listtest.c").contentAsString)


    checkResults2(allCode)
  }
}

class VectorTest extends StandardTest {
  "vector test" should "print the correct results" in {

    val allCode =  Seq(File("tests\\scala\\c\\engine\\libds-master\\vector.c").contentAsString,
                       File("tests\\scala\\c\\engine\\libds-master\\vectest.c").contentAsString)


    checkResults2(allCode)
  }
}

class HeapTest extends StandardTest {
  "heap test" should "print the correct results" in {

    val allCode =  Seq(File("tests\\scala\\c\\engine\\libds-master\\vector.c").contentAsString,
      File("tests\\scala\\c\\engine\\libds-master\\heap.c").contentAsString,
      File("tests\\scala\\c\\engine\\libds-master\\heaptest.c").contentAsString)


    checkResults2(allCode)
  }
}

class HashMapTest extends StandardTest {
  "heap test" should "print the correct results" in {

    val allCode =  Seq(File("tests\\scala\\c\\engine\\libds-master\\vector.c").contentAsString,
      File("tests\\scala\\c\\engine\\libds-master\\hashmap.c").contentAsString,
      File("tests\\scala\\c\\engine\\libds-master\\maptest.c").contentAsString)


    checkResults2(allCode)
  }
}

class RegexTest extends StandardTest {
  "A simple regex" should "print the correct results" in {

    val code =
      """
      #include <stdio.h>
      #include <stdlib.h>
      #include <string.h>
      #include "slre.h"

      void main() {
       struct slre_cap caps[10];
//       printf("%d\n", slre_match("^", "abcd", 4, NULL, 0, 0) == 0);
//       printf("%d\n", slre_match("x|^", "abcd", 4, NULL, 0, 0) == 0);
//       printf("%d\n", slre_match("x|$", "abcd", 4, NULL, 0, 0) == 4);
//       printf("%d\n", slre_match("x", "abcd", 4, NULL, 0, 0) == SLRE_NO_MATCH);
//       printf("%d\n", slre_match(".", "abcd", 4, NULL, 0, 0) == 1);
//       printf("%d\n", slre_match("^.*\\\\.*$", "c:\\Tools", 8, NULL, 0, SLRE_IGNORE_CASE) == 8);
//       printf("%d\n", slre_match("\\", "a", 1, NULL, 0, 0) == SLRE_INVALID_METACHARACTER);
//       printf("%d\n", slre_match("\\x", "a", 1, NULL, 0, 0) == SLRE_INVALID_METACHARACTER);
//       printf("%d\n", slre_match("\\x1", "a", 1, NULL, 0, 0) == SLRE_INVALID_METACHARACTER);
//       printf("%d\n", slre_match("\\x20", " ", 1, NULL, 0, 0) == 1);

//       printf("%d\n", slre_match("^.+$", "", 0, NULL, 0, 0) == SLRE_NO_MATCH);
       printf("%d\n", slre_match("^(.+)$", "", 0, NULL, 0, 0) == SLRE_NO_MATCH);
//       printf("%d\n", slre_match("^([\\+-]?)([\\d]+)$", "+", 1, caps, 10, SLRE_IGNORE_CASE) == SLRE_NO_MATCH);
//       printf("%d\n", slre_match("^([\\+-]?)([\\d]+)$", "+27", 3, caps, 10, SLRE_IGNORE_CASE) == 3);
//       printf("%d\n", caps[0].len == 1);
//       printf("%d\n", caps[0].ptr[0] == '+');
//       printf("%d\n", caps[1].len == 2);
//       printf("%d\n", memcmp(caps[1].ptr, "27", 2) == 0);
      }"""




    val allCode =  Seq(File("tests\\scala\\c\\engine\\libds-master\\slre.c").contentAsString, code)

    checkResults2(allCode, true)
  }
}