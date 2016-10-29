package scala.astViewer

import app.astViewer.Gcc
import app.astViewer.State

class ApiTest extends StandardTest {
  "interp test one" should "print the correct results" in {
    
    val state = new State
    
    Gcc.runCode("int i = 1432;", state)
    Gcc.runCode("printf(\"%d\\n\", i);", state)

  }
}