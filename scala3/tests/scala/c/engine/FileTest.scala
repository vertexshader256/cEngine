package scala.c.engine

import java.io.File

class FileTest extends StandardTest {

	"file read test" should "print the correct results" in {

		import java.io._
		val pw = new PrintWriter(new File("file.txt"))
		pw.write("Hello world!\n")
		pw.close

		val code =
			"""

      #include <stdio.h>

      void main() {
        FILE *fp;
        char buffer[100] = {0};
			  char *arr = "ok";

        /* Open file for both reading and writing */
        fp = fopen("file.txt", "r");

        /* Read and display data */
        fread(buffer, 1, 5, fp);
        printf("%s\n", buffer);

        fread(buffer, 1, 2, fp);
        printf("%s\n", buffer);

				int closed = fclose(fp);
			  printf("%d\n", closed);
      }"""

		checkResults(code, runConcurrent = false).map: result =>
			new File("file.txt").delete() // this file was created outside the test, delete after the test
			result
	}

	"create and write file test" should "print the correct results" in {

		val code =
			"""

      #include <stdio.h>

      void main() {
          FILE *passwd_text=fopen("filecreatetest.txt", "w");
          int rec_num;
          fprintf(passwd_text, "test!\n");
          fclose(passwd_text);

          char buff[100];
          FILE *f = fopen("filecreatetest.txt", "r");
          fgets(buff, 100, f);
          printf("String read: %s\n", buff);
          fclose(f);
					int wasDeleted = remove("filecreatetest.txt");
					printf("%d\n", wasDeleted);
      }"""

		checkResults(code, runConcurrent = false)
	}

	"file which doesnt exist" should "print the correct results" in {

		val code =
			"""
      #include <stdio.h>

      void main() {
          char buff[100];
          FILE *f = fopen("fsdfsdf.txt", "r");
          printf("file opened: %d\n", f);
          fclose(f);
					int wasDeleted = remove("fsdfsdf.txt");
					printf("%d\n", wasDeleted);
      }"""

		checkResults(code, runConcurrent = false)
	}

	"file existence check" should "print the correct results" in {

		import java.util.UUID.randomUUID
		val rand = randomUUID.toString.take(8)
		val rand2 = randomUUID.toString.take(8)

		val code =
			"""

      #include <stdio.h>

      void main() {
          FILE *nothere = fopen("""" + rand +
				""".txt", "r");

          if (!nothere) {
            printf("NOT HERE\n");
          } else {
            printf("HERE\n");
          }

          FILE *nothere2 = fopen("""" + rand2 +
				""".txt", "w");

          if (!nothere2) {
            printf("NOT HERE\n");
          } else {
            printf("HERE\n");
          }

					fclose(nothere2);
          int wasDeleted = remove("""" + rand2 +
				""".txt");
					printf("%d\n", wasDeleted);
      }"""

		checkResults(code, runConcurrent = false)
	}
}
