package tests.scala

import tests.scala.TestClasses._
import scala.c_engine._
import scala.c_engine.cEngine._

class ApiTest extends StandardTest {
  "interp test one" should "print the correct results" in {
    
    implicit val state = new State
    
    c"""int i = 1432;"""
    c"""printf("%d\n", i);"""
    c"""
      float x = 2.5f;
      printf("what: %.1f\n", x);
      
      """
      assert(state.stdout.toSeq == (Seq("1432", "what: 2.5")))
  }
}