package scala.astViewer

import better.files._

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
  
  "printing a string with inline pointer arithmetic" should "print the correct results" in {
    val code = """
      void main() {
        char str[] = "Hello!\n";
        printf("%s", str);
        printf("%s", str + 1);
      }"""

    checkResults(code)
  }
}

class printfCustom extends StandardTest {
  "hello world with custom print function" should "print the correct results" in {
    val code = """
      void main() {
        int x = 101;
        ee_printf("Hello World!\n");
        ee_printf("%d\n", 10);
        ee_printf("%d %d\n", 10, 15);
        ee_printf("%d\n", x);
      }"""

    checkResults(File("app\\ee_printf.c").contentAsString + "\n" + code)
  }
}