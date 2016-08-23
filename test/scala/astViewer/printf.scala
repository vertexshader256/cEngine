package scala.astViewer

class printf extends StandardTest {
    
  "Different basic prints" should "print the correct results" in {
    val code = """
      void main() {
        printf("Hello World!\n");
        printf("%s %s\n", "Hello", "World!");
        printf("%d\n", 1);
        printf("%s\n", "Hello World!");
        printf("%.2f\n", 1.47453);
        printf("%.1f\n", 1.47453);
      }"""

    checkResults(code)
  }
  
  "printing a string" should "print the correct results" in {
    val code = """
      void main() {
        char str[] = "Hello!\n";
        printf("%s", str);
        //printf("%s", &(str[0]));
      }"""

    checkResults(code)
  }
  
//  "printing a string" should "print the correct results" in {
//    val code = """
//      void main() {
//        char str[] = "Hello!\n";
//        printf("%s", str);
//      }"""
//
//    checkResults(code)
//  }
}
