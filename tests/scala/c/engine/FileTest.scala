package scala.c.engine

import java.io.{BufferedWriter, FileWriter}
import java.nio.file.Paths

class FileTest extends StandardTest {



  "file test" should "print the correct results" in {

    import java.io._
    val pw = new PrintWriter(new File("file.txt" ))
    pw.write("Hello world!\n")
    pw.close

    val code = """

      #include <stdio.h>

      void main() {
        FILE *fp;
        char buffer[100] = {0};

        /* Open file for both reading and writing */
        fp = fopen("file.txt", "r");

        /* Read and display data */
        fread(buffer, 1, 5, fp);
        printf("%s", buffer);

        fread(buffer, 1, 2, fp);
        printf("%s", buffer);
      }"""

    checkResults(code).map{result =>
      new File("file.txt" ).delete()
      result
    }
  }
}
