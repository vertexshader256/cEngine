package scala.c.engine

import java.nio.file.Paths

import better.files.File

class ListTest extends StandardTest {
  "list test" should "print the correct results" in {

    val list = Paths.get("tests", "scala", "c", "engine", "libds-master", "list.c")
    val listTest = Paths.get("tests", "scala", "c", "engine", "libds-master", "listtest.c")

    val allCode = Seq(File(list).contentAsString, File(listTest).contentAsString)

    checkResults2(allCode)
  }
}

class VectorTest extends StandardTest {
  "vector test" should "print the correct results" in {

    val vector = Paths.get("tests", "scala", "c", "engine", "libds-master", "vector.c")
    val vecTest = Paths.get("tests", "scala", "c", "engine", "libds-master", "vectest.c")

    val allCode =  Seq(File(vector).contentAsString, File(vecTest).contentAsString)

    checkResults2(allCode)
  }
}

class HeapTest extends StandardTest {
  "heap test" should "print the correct results" in {

    val vector = Paths.get("tests", "scala", "c", "engine", "libds-master", "vector.c")
    val heap = Paths.get("tests", "scala", "c", "engine", "libds-master", "heap.c")
    val heapTest = Paths.get("tests", "scala", "c", "engine", "libds-master", "heaptest.c")

    val allCode =  Seq(File(vector).contentAsString, File(heap).contentAsString,
      File(heapTest).contentAsString)

    checkResults2(allCode)
  }
}

class HashMapTest extends StandardTest {
  "heap test" should "print the correct results" in {

    val vector = Paths.get("tests", "scala", "c", "engine", "libds-master", "vector.c")
    val hashmap = Paths.get("tests", "scala", "c", "engine", "libds-master", "hashmap.c")
    val maptest = Paths.get("tests", "scala", "c", "engine", "libds-master", "maptest.c")

    val allCode =  Seq(File(vector).contentAsString, File(hashmap).contentAsString,
      File(maptest).contentAsString)

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
      #include <assert.h>
      #include "slre.h"

      #define ASSERT(expr) if (expr) { printf("PASS\n"); } else { printf("FAIL\n"); }

      void main() {
       struct slre_cap caps[10];
       /* Metacharacters */
          ASSERT(slre_match("$", "abcd", 4, NULL, 0, 0) == 4);
          ASSERT(slre_match("^", "abcd", 4, NULL, 0, 0) == 0);
          ASSERT(slre_match("x|^", "abcd", 4, NULL, 0, 0) == 0);
          ASSERT(slre_match("x|$", "abcd", 4, NULL, 0, 0) == 4);
          ASSERT(slre_match("x", "abcd", 4, NULL, 0, 0) == SLRE_NO_MATCH);
          ASSERT(slre_match(".", "abcd", 4, NULL, 0, 0) == 1);
          ASSERT(slre_match("^.*\\\\.*$", "c:\\Tools", 8, NULL, 0, SLRE_IGNORE_CASE)
            == 8);
          ASSERT(slre_match("\\", "a", 1, NULL, 0, 0) == SLRE_INVALID_METACHARACTER);
          ASSERT(slre_match("\\x", "a", 1, NULL, 0, 0) == SLRE_INVALID_METACHARACTER);
          ASSERT(slre_match("\\x1", "a", 1, NULL, 0, 0) == SLRE_INVALID_METACHARACTER);
          ASSERT(slre_match("\\x20", " ", 1, NULL, 0, 0) == 1);

          ASSERT(slre_match("^.+$", "", 0, NULL, 0, 0) == SLRE_NO_MATCH);
          ASSERT(slre_match("^(.+)$", "", 0, NULL, 0, 0) == SLRE_NO_MATCH);
          ASSERT(slre_match("^([\\+-]?)([\\d]+)$", "+", 1,
                            caps, 10, SLRE_IGNORE_CASE) == SLRE_NO_MATCH);
          ASSERT(slre_match("^([\\+-]?)([\\d]+)$", "+27", 3,
                            caps, 10, SLRE_IGNORE_CASE) == 3);
          ASSERT(caps[0].len == 1);
          ASSERT(caps[0].ptr[0] == '+');
          ASSERT(caps[1].len == 2);
          ASSERT(memcmp(caps[1].ptr, "27", 2) == 0);

          ASSERT(slre_match("tel:\\+(\\d+[\\d-]+\\d)",
                            "tel:+1-201-555-0123;a=b", 23, caps, 10, 0) == 19);
          ASSERT(caps[0].len == 14);
          ASSERT(memcmp(caps[0].ptr, "1-201-555-0123", 14) == 0);

          /* Character sets */
          ASSERT(slre_match("[abc]", "1c2", 3, NULL, 0, 0) == 2);
          ASSERT(slre_match("[abc]", "1C2", 3, NULL, 0, 0) == SLRE_NO_MATCH);
          ASSERT(slre_match("[abc]", "1C2", 3, NULL, 0, SLRE_IGNORE_CASE) == 2);
          ASSERT(slre_match("[.2]", "1C2", 3, NULL, 0, 0) == 1);
          ASSERT(slre_match("[\\S]+", "ab cd", 5, NULL, 0, 0) == 2);
          ASSERT(slre_match("[\\S]+\\s+[tyc]*", "ab cd", 5, NULL, 0, 0) == 4);
          ASSERT(slre_match("[\\d]", "ab cd", 5, NULL, 0, 0) == SLRE_NO_MATCH);
          ASSERT(slre_match("[^\\d]", "ab cd", 5, NULL, 0, 0) == 1);
          ASSERT(slre_match("[^\\d]+", "abc123", 6, NULL, 0, 0) == 3);
          ASSERT(slre_match("[1-5]+", "123456789", 9, NULL, 0, 0) == 5);
          ASSERT(slre_match("[1-5a-c]+", "123abcdef", 9, NULL, 0, 0) == 6);
          ASSERT(slre_match("[1-5a-]+", "123abcdef", 9, NULL, 0, 0) == 4);
          ASSERT(slre_match("[1-5a-]+", "123a--2oo", 9, NULL, 0, 0) == 7);
          ASSERT(slre_match("[htps]+://", "https://", 8, NULL, 0, 0) == 8);
          ASSERT(slre_match("[^\\s]+", "abc def", 7, NULL, 0, 0) == 3);
          ASSERT(slre_match("[^fc]+", "abc def", 7, NULL, 0, 0) == 2);
          ASSERT(slre_match("[^d\\sf]+", "abc def", 7, NULL, 0, 0) == 3);

          /* Flags - case sensitivity */
          ASSERT(slre_match("FO", "foo", 3, NULL, 0, 0) == SLRE_NO_MATCH);
          ASSERT(slre_match("FO", "foo", 3, NULL, 0, SLRE_IGNORE_CASE) == 2);
          ASSERT(slre_match("(?m)FO", "foo", 3, NULL, 0, 0) ==
            SLRE_UNEXPECTED_QUANTIFIER);
          ASSERT(slre_match("(?m)x", "foo", 3, NULL, 0, 0) ==
            SLRE_UNEXPECTED_QUANTIFIER);

          ASSERT(slre_match("fo", "foo", 3, NULL, 0, 0) == 2);
          ASSERT(slre_match(".+", "foo", 3, NULL, 0, 0) == 3);
          ASSERT(slre_match(".+k", "fooklmn", 7, NULL, 0, 0) == 4);
          ASSERT(slre_match(".+k.", "fooklmn", 7, NULL, 0, 0) == 5);
          ASSERT(slre_match("p+", "fooklmn", 7, NULL, 0, 0) == SLRE_NO_MATCH);
          ASSERT(slre_match("ok", "fooklmn", 7, NULL, 0, 0) == 4);
          ASSERT(slre_match("lmno", "fooklmn", 7, NULL, 0, 0) == SLRE_NO_MATCH);
          ASSERT(slre_match("mn.", "fooklmn", 7, NULL, 0, 0) == SLRE_NO_MATCH);
          ASSERT(slre_match("o", "fooklmn", 7, NULL, 0, 0) == 2);
          ASSERT(slre_match("^o", "fooklmn", 7, NULL, 0, 0) == SLRE_NO_MATCH);
          ASSERT(slre_match("^", "fooklmn", 7, NULL, 0, 0) == 0);
          ASSERT(slre_match("n$", "fooklmn", 7, NULL, 0, 0) == 7);
          ASSERT(slre_match("n$k", "fooklmn", 7, NULL, 0, 0) == SLRE_NO_MATCH);
          ASSERT(slre_match("l$", "fooklmn", 7, NULL, 0, 0) == SLRE_NO_MATCH);
          ASSERT(slre_match(".$", "fooklmn", 7, NULL, 0, 0) == 7);
          ASSERT(slre_match("a?", "fooklmn", 7, NULL, 0, 0) == 0);
          ASSERT(slre_match("^a*CONTROL", "CONTROL", 7, NULL, 0, 0) == 7);
          ASSERT(slre_match("^[a]*CONTROL", "CONTROL", 7, NULL, 0, 0) == 7);
          ASSERT(slre_match("^(a*)CONTROL", "CONTROL", 7, NULL, 0, 0) == 7);
          ASSERT(slre_match("^(a*)?CONTROL", "CONTROL", 7, NULL, 0, 0) == 7);

          ASSERT(slre_match("\\_", "abc", 3, NULL, 0, 0) == SLRE_INVALID_METACHARACTER);
          ASSERT(slre_match("+", "fooklmn", 7, NULL, 0, 0) == SLRE_UNEXPECTED_QUANTIFIER);
          ASSERT(slre_match("()+", "fooklmn", 7, NULL, 0, 0) == SLRE_NO_MATCH);
          ASSERT(slre_match("\\x", "12", 2, NULL, 0, 0) == SLRE_INVALID_METACHARACTER);
          ASSERT(slre_match("\\xhi", "12", 2, NULL, 0, 0) == SLRE_INVALID_METACHARACTER);
          ASSERT(slre_match("\\x20", "_ J", 3, NULL, 0, 0) == 2);
          ASSERT(slre_match("\\x4A", "_ J", 3, NULL, 0, 0) == 3);
          ASSERT(slre_match("\\d+", "abc123def", 9, NULL, 0, 0) == 6);

          /* Balancing brackets */
          ASSERT(slre_match("(x))", "fooklmn", 7, NULL, 0, 0) == SLRE_UNBALANCED_BRACKETS);
          ASSERT(slre_match("(", "fooklmn", 7, NULL, 0, 0) == SLRE_UNBALANCED_BRACKETS);

          ASSERT(slre_match("klz?mn", "fooklmn", 7, NULL, 0, 0) == 7);
          ASSERT(slre_match("fa?b", "fooklmn", 7, NULL, 0, 0) == SLRE_NO_MATCH);

          /* Brackets & capturing */
          ASSERT(slre_match("^(te)", "tenacity subdues all", 20, caps, 10, 0) == 2);
          ASSERT(slre_match("(bc)", "abcdef", 6, caps, 10, 0) == 3);
          ASSERT(slre_match(".(d.)", "abcdef", 6, caps, 10, 0) == 5);
          ASSERT(slre_match(".(d.)\\)?", "abcdef", 6, caps, 10, 0) == 5);
          ASSERT(caps[0].len == 2);
          ASSERT(memcmp(caps[0].ptr, "de", 2) == 0);
          ASSERT(slre_match("(.+)", "123", 3, caps, 10, 0) == 3);
          ASSERT(slre_match("(2.+)", "123", 3, caps, 10, 0) == 3);
          ASSERT(caps[0].len == 2);
          ASSERT(memcmp(caps[0].ptr, "23", 2) == 0);
          ASSERT(slre_match("(.+2)", "123", 3, caps, 10, 0) == 2);
          ASSERT(caps[0].len == 2);
          ASSERT(memcmp(caps[0].ptr, "12", 2) == 0);
          ASSERT(slre_match("(.*(2.))", "123", 3, caps, 10, 0) == 3);
          ASSERT(slre_match("(.)(.)", "123", 3, caps, 10, 0) == 2);
          ASSERT(slre_match("(\\d+)\\s+(\\S+)", "12 hi", 5, caps, 10, 0) == 5);
          ASSERT(slre_match("ab(cd)+ef", "abcdcdef", 8, NULL, 0, 0) == 8);
          ASSERT(slre_match("ab(cd)*ef", "abcdcdef", 8, NULL, 0, 0) == 8);
          ASSERT(slre_match("ab(cd)+?ef", "abcdcdef", 8, NULL, 0, 0) == 8);
          ASSERT(slre_match("ab(cd)+?.", "abcdcdef", 8, NULL, 0, 0) == 5);
          ASSERT(slre_match("ab(cd)?", "abcdcdef", 8, NULL, 0, 0) == 4);
          ASSERT(slre_match("a(b)(cd)", "abcdcdef", 8, caps, 1, 0) ==
              SLRE_CAPS_ARRAY_TOO_SMALL);
          ASSERT(slre_match("(.+/\\d+\\.\\d+)\\.jpg$", "/foo/bar/12.34.jpg", 18,
                            caps, 1, 0) == 18);
          ASSERT(slre_match("(ab|cd).*\\.(xx|yy)", "ab.yy", 5, NULL, 0, 0) == 5);
          ASSERT(slre_match(".*a", "abcdef", 6, NULL, 0, 0) == 1);
          ASSERT(slre_match("(.+)c", "abcdef", 6, NULL, 0, 0) == 3);
          ASSERT(slre_match("\\n", "abc\ndef", 7, NULL, 0, 0) == 4);
          ASSERT(slre_match("b.\\s*\\n", "aa\r\nbb\r\ncc\r\n\r\n", 14,
                            caps, 10, 0) == 8);

          /* Greedy vs non-greedy */
          ASSERT(slre_match(".+c", "abcabc", 6, NULL, 0, 0) == 6);
          ASSERT(slre_match(".+?c", "abcabc", 6, NULL, 0, 0) == 3);
          ASSERT(slre_match(".*?c", "abcabc", 6, NULL, 0, 0) == 3);
          ASSERT(slre_match(".*c", "abcabc", 6, NULL, 0, 0) == 6);
          ASSERT(slre_match("bc.d?k?b+", "abcabc", 6, NULL, 0, 0) == 5);

          /* Branching */
          ASSERT(slre_match("|", "abc", 3, NULL, 0, 0) == 0);
          ASSERT(slre_match("|.", "abc", 3, NULL, 0, 0) == 1);
          ASSERT(slre_match("x|y|b", "abc", 3, NULL, 0, 0) == 2);

          ASSERT(slre_match("k(xx|yy)|ca", "abcabc", 6, NULL, 0, 0) == 4);
          ASSERT(slre_match("k(xx|yy)|ca|bc", "abcabc", 6, NULL, 0, 0) == 3);

          ASSERT(slre_match("(|.c)", "abc", 3, caps, 10, 0) == 3);
          ASSERT(caps[0].len == 2);
          ASSERT(memcmp(caps[0].ptr, "bc", 2) == 0);
          ASSERT(slre_match("a|b|c", "a", 1, NULL, 0, 0) == 1);
          ASSERT(slre_match("a|b|c", "b", 1, NULL, 0, 0) == 1);
          ASSERT(slre_match("a|b|c", "c", 1, NULL, 0, 0) == 1);
          ASSERT(slre_match("a|b|c", "d", 1, NULL, 0, 0) == SLRE_NO_MATCH);

          /* Optional match at the end of the string */
          ASSERT(slre_match("^.*c.?$", "abc", 3, NULL, 0, 0) == 3);
          ASSERT(slre_match("^.*C.?$", "abc", 3, NULL, 0, SLRE_IGNORE_CASE) == 3);
          ASSERT(slre_match("bk?", "ab", 2, NULL, 0, 0) == 2);
          ASSERT(slre_match("b(k?)", "ab", 2, NULL, 0, 0) == 2);
          ASSERT(slre_match("b[k-z]*", "ab", 2, NULL, 0, 0) == 2);
          ASSERT(slre_match("ab(k|z|y)*", "ab", 2, NULL, 0, 0) == 2);
          ASSERT(slre_match("[b-z].*", "ab", 2, NULL, 0, 0) == 2);
          ASSERT(slre_match("(b|z|u).*", "ab", 2, NULL, 0, 0) == 2);
          ASSERT(slre_match("ab(k|z|y)?", "ab", 2, NULL, 0, 0) == 2);
          ASSERT(slre_match(".*", "ab", 2, NULL, 0, 0) == 2);
          ASSERT(slre_match(".*$", "ab", 2, NULL, 0, 0) == 2);
          ASSERT(slre_match("a+$", "aa", 2, NULL, 0, 0) == 2);
          ASSERT(slre_match("a*$", "aa", 2, NULL, 0, 0) == 2);
          ASSERT(slre_match( "a+$" ,"Xaa", 3, NULL, 0, 0) == 3);
          ASSERT(slre_match( "a*$" ,"Xaa", 3, NULL, 0, 0) == 3);

          /* Ignore case flag */
          ASSERT(slre_match("[a-h]+", "abcdefghxxx", 11, NULL, 0, 0) == 8);
          ASSERT(slre_match("[A-H]+", "ABCDEFGHyyy", 11, NULL, 0, 0) == 8);
          ASSERT(slre_match("[a-h]+", "ABCDEFGHyyy", 11, NULL, 0, 0) == SLRE_NO_MATCH);
          ASSERT(slre_match("[A-H]+", "abcdefghyyy", 11, NULL, 0, 0) == SLRE_NO_MATCH);
          ASSERT(slre_match("[a-h]+", "ABCDEFGHyyy", 11, NULL, 0, SLRE_IGNORE_CASE) == 8);
          ASSERT(slre_match("[A-H]+", "abcdefghyyy", 11, NULL, 0, SLRE_IGNORE_CASE) == 8);
      }"""

    val slre = Paths.get("tests", "scala", "c", "engine", "libds-master", "slre.c")

    val allCode =  Seq(File(slre).contentAsString, code)

    checkResults2(allCode)
  }
}