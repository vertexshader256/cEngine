package scala.astViewer

import app.astViewer.Gcc
import app.astViewer.State
import app.astViewer.cEngine._
import better.files._

class DataStructuresTest extends StandardTest {
  "list test" should "print the correct results" in {

    val allCode =  Seq(File("test\\scala\\libds-master\\list.c").contentAsString,
                       File("test\\scala\\libds-master\\listtest.c").contentAsString)
                          
                        
    checkResults(allCode)
  }
  
//  "vector test" should "print the correct results" in {
//
//    val allCode =  Seq(File("test\\scala\\libds-master\\vector.c").contentAsString,
//                       File("test\\scala\\libds-master\\vectest.c").contentAsString)
//                          
//                        
//    checkResults(allCode)
//  }
}