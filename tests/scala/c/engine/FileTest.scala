package scala.c.engine

import java.io.{BufferedWriter, File, FileWriter}
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

  "create file test" should "print the correct results" in {

    val code = """

      #include <stdio.h>

      void main() {
          FILE *passwd_text=fopen("passwd.txt", "w");
          int rec_num;
          fprintf(passwd_text, "test!\n");
          fclose(passwd_text);

          char buff[100];
          FILE *f = fopen("passwd.txt", "r");
          fgets(buff, 100, f);
          printf("String read: %s\n", buff);
          fclose(f);

      }"""

    checkResults(code).map{result =>
      new File("passwd.txt" ).delete()
      result
    }
  }
}
