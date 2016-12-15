package scala.astViewer

import app.astViewer.Gcc
import app.astViewer.State
import app.astViewer.cEngine._
import better.files._

class DataStructuresTest extends StandardTest {
  "interp test one" should "print the correct results" in {
    
    val dataStructureFiles = File(raw"C:\Scala\Git\astViewer\test\scala\c-algorithms-master\src").children.filter(_.extension == Some(".c")).map(_.contentAsString)

    val dataCode = dataStructureFiles.reduce{_ + "\n" + _}
    
    val allCode =  File("test\\scala\\c-algorithms-master\\src\\arraylist.c").contentAsString +
                          "\n" + File("test\\scala\\c-algorithms-master\\src\\compare-int.c").contentAsString +
                          "\n" + File("test\\scala\\c-algorithms-master\\test\\alloc-testing.c").contentAsString +
                          "\n" + File("test\\scala\\c-algorithms-master\\test\\framework.c").contentAsString +
                          "\n" + File("test\\scala\\c-algorithms-master\\test\\test-arraylist.c").contentAsString
    
     File("what.txt").write(allCode)                     
                          
    checkResults(allCode)
  }
}