package scala.astViewer

import org.scalatest._

class ForLooptest extends FlatSpec with ShouldMatchers {
  "A simple for loop" should "print the correct results" in {
    val code = """
      void main() {
        int x = 0;
        int i = 0;
        for (i = 0; i < 10; i = i + 1) {
          x = x + 1;
        }
        
        printf("%d\n", x);
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("10"))
  }
  
  "A little more advanced for loop" should "print the correct results" in {
    val code = """
      void main() {
        int x = 0;
        for (int i = 0; i < 10; i = i + 1) {
          x = x + 1;
        }
        
        printf("%d\n", x);
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("10"))
  }
  
  "A for loop with a function call" should "print the correct results" in {
    val code = """
      
      int blah = 0;
      
      void increment() {
        blah++;
      }
      
      void main() {
        int x = 0;
        for (int i = 0; i < 10; i += 2) {
          increment();
        }
        
        printf("%d\n", blah);
      }"""

    val executor = new Executor(code)
    executor.execute
    executor.stdout.headOption should equal (Some("5"))
  }
  
  "A for loop setting an array" should "print the correct results" in {
    val code = """

      void main() {
        int x[5];
        for (int i = 0; i < 5; i++) {
          x[i] = i;
        }
        
        
        printf("%d\n", x[3]);
      }"""

    val executor = new Executor(code)
    executor.execute
    
    executor.stdout.head should equal ("3")
  }
}