package scala.astViewer

import better.files._

class printf extends StandardTest {
    
//  "Different basic prints" should "print the correct results" in {
//    val code = """
//      void main() {
//        printf("Hello World!\n");
//        printf("%s %s\n", "Hello", "World!");
//        printf("%d\n", 1);
//        printf("%s\n", "Hello World!");
//        printf("%.2f\n", 1.47453);
//        printf("%.1f\n", 1.47453);
//      }"""
//
//    checkResults(code)
//  }
  
  
  
  "more advanced printing with custom print function" should "print the correct results" in {
    val code = """
      void main() {
        ee_printf("%s %s\n", "Hello", "World!");
//        ee_printf("%d\n", 1);
//        ee_printf("%s\n", "Hello World!");
//        ee_printf("%.2f\n", 1.47453);
//        ee_printf("%.1f\n", 1.47453);
      }"""

    checkResults(File("app\\ee_printf.c").contentAsString + "\n" + code)
  }
  
//  "printing a string with inline pointer arithmetic" should "print the correct results" in {
//    val code = """
//      void main() {
//        char str[] = "Hello!\n";
//        printf("%s", str);
//        printf("%s", str + 1);
//      }"""
//
//    checkResults(code)
//  }
}

class printfCustom extends StandardTest {
  "hello world with custom print function" should "print the correct results" in {
    val code = """
      void main() {
        ee_printf("Hello World!\n");
      }"""

    checkResults(File("app\\ee_printf.c").contentAsString + "\n" + code)
  }
}