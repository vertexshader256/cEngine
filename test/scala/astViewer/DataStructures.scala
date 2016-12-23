package scala.astViewer

import app.astViewer.Gcc
import app.astViewer.State
import app.astViewer.cEngine._
import better.files._

class DataStructuresTest extends StandardTest {
  "interp test one" should "print the correct results" in {
    
    val dataStructureFiles = File(raw"test\\scala\\c-algorithms-master\\src").children.filter(_.extension == Some(".c")).map(_.contentAsString)

    val dataCode = dataStructureFiles.reduce{_ + "\n" + _}
    
    val allCode =  Seq(File("test\\scala\\c-algorithms-master\\test\\alloc-testing.c").contentAsString,
                       File("test\\scala\\c-algorithms-master\\src\\arraylist.c").contentAsString,
                       File("test\\scala\\c-algorithms-master\\src\\compare-int.c").contentAsString,
                       File("test\\scala\\c-algorithms-master\\test\\framework.c").contentAsString,
                       File("test\\scala\\c-algorithms-master\\test\\test-arraylist.c").contentAsString)
                          
                        
    checkResults(allCode)
  }
}