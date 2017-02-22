package scala.astViewer

import app.astViewer.Gcc
import app.astViewer.State
import app.astViewer.cEngine._

class ApiTest extends StandardTest {
  "interp test one" should "print the correct results" in {
    
    implicit val state = new State
    
    c"""int i = 1432;"""
    c"""printf("%d\n", i);"""
    c"""
      float x = 2.5f;
      printf("what: %.1f\n", x);
      
      """

    state.stdout.toSeq should equal (Seq("1432", "what: 2.5"))
  }
}