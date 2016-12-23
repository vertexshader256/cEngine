package scala.astViewer

import app.astViewer.Gcc
import app.astViewer.State
import app.astViewer.cEngine._
import better.files._

class DataStructuresTest extends StandardTest {
  "interp test one" should "print the correct results" in {

    val allCode =  Seq(File("test\\scala\\libds-master\\list.c").contentAsString,
                       File("test\\scala\\libds-master\\listtest.c").contentAsString)
                          
                        
    checkResults(allCode)
  }
}