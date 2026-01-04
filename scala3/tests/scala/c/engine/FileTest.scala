package scala.c.engine

import java.io.File

class FileTest extends StandardTest {

	"file test" should "print the correct results" in {

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
        printf("%s", buffer);

        fread(buffer, 1, 2, fp);
        printf("%s", buffer);
      }"""

		checkResults(code, runConcurrent = false).map { result =>
			new File("file.txt").delete()
			result
		}
	}

	"file write test" should "print the correct results" in {

		import java.io._
		val file = new File("filewritetest.txt")
		val fileName = file.getName
		val pw = new PrintWriter(file)
		pw.write("zzzzzzzzzzzzzzzzzz\n")
		pw.close

		val code =
			s"""

				#include <stdio.h>

				void main() {
					FILE *fp;
					char buffer[100] = {0};
					char *arr = "ok43234";

					/* Open file for both reading and writing */
					fp = fopen("$fileName", "w");
		 			printf("%s\\n", buffer);

					printf("%s", arr);
		 			fwrite(arr, 1, 2, fp);

				  if (fclose(fp) == 0) {
						printf("File closed successfully.\\n");
					} else {
						printf("Error closing file\\n");
					}

					fread(buffer, 1, 2, fp);
					printf("%s\\n", buffer);
		      printf("%s\\n", "done");
		      remove("$fileName");
				}"""

		checkResults(code, runConcurrent = false).map { result =>
			file.delete()
			result
		}
	}

	"create file test" should "print the correct results" in {

		val code =
			"""

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

		checkResults(code, runConcurrent = false).map { result =>
			new File("passwd.txt").delete()
			result
		}
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

          remove("""" + rand2 +
				""".txt");

      }"""

		checkResults(code, runConcurrent = false).map { result =>
			result
		}
	}
}
