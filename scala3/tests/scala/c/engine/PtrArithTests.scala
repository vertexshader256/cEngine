package scala.c.engine

class PointerArithmeticTest extends StandardTest2("pointer arithmetic on a pointer type",
	"""
      void main() {
        int num[10] = {1,2,3,4,5,6,7,8,9,10};
        int *arr = num;
        int y = 5;

        int *p1 = arr + 3;
        int *p2 = 5 + arr;
        int p3 = p2 - p1;
        int p4 = p1 - p2;
        int p5 = p1 + 5 - p2;

        int *p6 = arr + y;
        int *p7 = y + arr;

        printf("%d %d %d %d %d %d %d\n", *p1, *p2, p3, p4, p5, *p6, *p7);
      }"""
)

class PointerArithmeticTest2 extends StandardTest2("pointer arithmetic between pointers",
	"""
      void main() {
         int* x[5];
         int y[5];

         printf("%d %d\n", &x[0] - &x[1], &x[1] - &x[0]);
         printf("%d %d\n", &x[0] - &x[4], &x[4] - &x[0]);

         y[0] = 5;
         int* z = y;
         *(z + 1) = 10;
         printf("%d\n", y[1]);

      }"""
)

class PointerArithmeticTest3 extends StandardTest2("pointer arithmetic with pointers to structs",
	"""
      void main() {
         struct Test2 {
             int x;
             int y;
             int z;
         };

         struct Test {
            int x;
            int y;
            int z;
            struct Test* ptr2;
            struct Test2 j[10];
         };

         struct Test x[5];
         struct Test* ptr = &x[1];
         ptr->ptr2 = &x[2];
         int i = 1;

         printf("%d\n", &x[0] - &x[1]);
         printf("%d\n", &x[0] - (&x[1] + 1));
         printf("%d\n", &x[0] - (&x[1] - 1));
         printf("%d\n", &x[0] - (1 + &x[1]));

         printf("%d\n", &x[0] - ptr);
         printf("%d\n", &x[0] - (ptr + 1));
         printf("%d\n", &x[0] - (ptr - 1));
         printf("%d\n", &x[0] - (1 + ptr));

         printf("%d\n", x - ptr);
         printf("%d\n", x - (ptr++ + 1));
         printf("%d\n", x - (++ptr - 1));
         printf("%d\n", x - (1 + ptr));

         printf("%d\n", &x[2] - x);
         printf("%d\n", &x[2] - (x + 1));
         printf("%d\n", &x[2] - (x - 1));
         printf("%d\n", &x[2] - (1 + x));

         printf("%d\n", ptr - x);
         printf("%d\n", ptr - (x + 1));
         printf("%d\n", ptr - (x - 1));
         printf("%d\n", ptr - (1 + x));

         printf("%d\n", ptr - (x + i));
         printf("%d\n", ptr - (x - i));
         printf("%d\n", ptr - (i + x));

         printf("%d\n", ptr + i - x);
         printf("%d\n", x + i - ++ptr);
         printf("%d\n", x + 2 - ptr);
      }"""
)

class PointerArithmeticTest4 extends StandardTest2("pointer arithmetic with pointers to typedef structs",
	"""
      void main() {
         typedef struct {
             int x;
             int y;
             int z;
         } Test2;

         typedef struct {
            int x;
            int y;
            int z;
            struct Test* ptr2;
            Test2 j[10];
         } Test;

         Test x[5];
         Test* ptr = &x[1];
         int i = 1;

         printf("%d\n", &x[0] - &x[1]);
         printf("%d\n", &x[0] - (&x[1] + 1));
         printf("%d\n", &x[0] - (&x[1] - 1));
         printf("%d\n", &x[0] - (1 + &x[1]));

         printf("%d\n", &x[0] - ptr);
         printf("%d\n", &x[0] - (ptr + 1));
         printf("%d\n", &x[0] - (ptr - 1));
         printf("%d\n", &x[0] - (1 + ptr));

         printf("%d\n", x - ptr);
         printf("%d\n", x - (ptr++ + 1));
         printf("%d\n", x - (++ptr - 1));
         printf("%d\n", x - (1 + ptr));

         printf("%d\n", &x[2] - x);
         printf("%d\n", &x[2] - (x + 1));
         printf("%d\n", &x[2] - (x - 1));
         printf("%d\n", &x[2] - (1 + x));

         printf("%d\n", ptr - x);
         printf("%d\n", ptr - (x + 1));
         printf("%d\n", ptr - (x - 1));
         printf("%d\n", ptr - (1 + x));

         printf("%d\n", ptr - (x + i));
         printf("%d\n", ptr - (x - i));
         printf("%d\n", ptr - (i + x));

         printf("%d\n", ptr + i - x);
         printf("%d\n", x + i - ++ptr);
         printf("%d\n", x + 2 - ptr);
      }"""
)

class PointerArithmeticTest5 extends StandardTest2("pointer arithmetic with pointers to arrays",
	"""
      void main() {
         typedef int Test[10];

         Test x[5];
         Test* ptr = &x[1];
         int i = 1;

         printf("%d\n", &x[0] - &x[1]);
         printf("%d\n", &x[0] - (&x[1] + 1));
         printf("%d\n", &x[0] - (&x[1] - 1));
         printf("%d\n", &x[0] - (1 + &x[1]));

         printf("%d\n", &x[0] - ptr);
         printf("%d\n", &x[0] - (ptr + 1));
         printf("%d\n", &x[0] - (ptr - 1));
         printf("%d\n", &x[0] - (1 + ptr));

         printf("%d\n", x - ptr);
         printf("%d\n", x - (ptr++ + 1));
         printf("%d\n", x - (++ptr - 1));
         printf("%d\n", x - (1 + ptr));

         printf("%d\n", &x[2] - x);
         printf("%d\n", &x[2] - (x + 1));
         printf("%d\n", &x[2] - (x - 1));
         printf("%d\n", &x[2] - (1 + x));

         printf("%d\n", ptr - x);
         printf("%d\n", ptr - (x + 1));
         printf("%d\n", ptr - (x - 1));
         printf("%d\n", ptr - (1 + x));

         printf("%d\n", ptr - (x + i));
         printf("%d\n", ptr - (x - i));
         printf("%d\n", ptr - (i + x));

         printf("%d\n", ptr + i - x);
         printf("%d\n", x + i - ++ptr);
         printf("%d\n", x + 2 - ptr);
      }"""
)

class PointerArithmeticTest6 extends StandardTest2("tricky pointer arithmetic with pointers case",
	"""
      void main() {

         typedef int Test[10];

         Test x[5];
         Test* ptr = &x[1];

         printf("%d\n", x - ++ptr);

      }"""
)

class PointerArithmeticTest7 extends StandardTest2("tricky pointer arithmetic with pointers case 2",
	"""
      void main() {
         int x[10][5];
         int (*ptr)[5] = &x[1];

         printf("%d\n", x - ++ptr);

      }"""
)

class PointerArithmeticTest8 extends StandardTest2("advanced pointer arithmetic",
	"""
       char *c[] = {"GeksQuiz", "MCQ", "TEST", "QUIZ"};
       char **cp[] = {c+3, c+2, c+1, c};
       char ***cpp = cp;

       int main()
       {
           printf("%s ", **++cpp);
           printf("%s ", *--*++cpp+3);
           printf("%s ", *cpp[-2]+3);
           printf("%s ", cpp[-1][-1]+1);
           return 0;
       }"""
)

class PointerArithmeticTest9 extends StandardTest2("advanced pointer arithmetic 2",
	"""
       int main()
       {
           int a[][3] = {1, 2, 3, 4, 5, 6};
           int (*ptr)[3] = a;
           printf("%d %d\n", (*ptr)[1], (*ptr)[2]);
           ++ptr;
           printf("%d %d\n", (*ptr)[1], (*ptr)[2]);
       }"""
)

class PointerArithmeticTest10 extends StandardTest2("advanced pointer arithmetic 5",
	"""
       int main()
       {
           int a[2][3] = {1, 2, 3, 4, 5, 6};
           int (*ptr)[3] = a;
           printf("%d %d\n", (*ptr)[1], (*ptr)[2]);
           ++ptr;
           printf("%d %d\n", (*ptr)[1], (*ptr)[2]);
       }"""
)

class PointerArithmeticTest11 extends StandardTest2("advanced pointer arithmetic 3",
	"""
       int fun(int arr[]) {
          arr = arr+1;
          printf("%d ", arr[0]);
       }
       int main(void) {
          int arr[2] = {10, 20};
          fun(arr);
          printf("%d", arr[0]);
          return 0;
       }"""
)

class PointerArithmeticTest12 extends StandardTest2("tricky pointer arithmetic with pointers case 2",
	"""
      void main() {
         char *blah = "hellothisisjustatest";
         long offset = 5;
         short offsetShort = 5;
         char offsetChar = 5;

         char *x = blah + offset;
         char *x2 = blah + 10L;
         char *x3 = blah + offsetShort;
         char *x4 = blah + offsetChar;

         printf("%s\n", x);
         printf("%s\n", x - offset);
         printf("%s\n", x - 5L);
         printf("%s\n", x3 - 5L);
         printf("%s\n", x4 - ((char)5));
         printf("%s\n", 5L + x);
         printf("%s\n", 5L + x3);
         printf("%s\n", ((char)5) + x4);
         printf("%d\n", x2 - x);

      }"""
)

class PointerArithmeticTest13 extends StandardTest2("pointer comparison",
	"""
			void main() {
				int arr[5];

				// declaring pointer to array name
				int* ptr1 = &arr;
				// declaring pointer to first element
				int* ptr2 = &arr[0];

				if (ptr1 == ptr2) {
						printf("Pointer to Array Name and First Element "
									 "are Equal.");
				}
				else {
						printf("Pointer to Array Name and First Element "
									 "are not Equal.");
				}
			}"""
)

class PointerArithmeticTest14 extends StandardTest2("pointer comparison",
	"""
			void main() {
				int n = 10;

					int arr[] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };

					// Declaration of pointer variable
					int* ptr;

					// Pointer points the first (0th index)
					// element in an array
					ptr = arr;
					int count_even = 0;
					int count_odd = 0;

					for (int i = 0; i < n; i++) {

							if (*ptr % 2 == 0) {
									count_even++;
							}
							if (*ptr % 2 != 0) {
									count_odd++;
							}

							// Pointing to the next
							// element in an array
							ptr++;
					}
					printf("No of even elements in an array is : %d", count_even);
					printf("\nNo of odd elements in an array is : %d",count_odd);
			}"""
)

class PointerArithmeticTest15 extends StandardTest2("pointer comparison",
	"""
			void main() {
				int N = 5;

				// An array
				int arr[] = { 1, 2, 3, 4, 5 };

				// Declare pointer variable
				int* ptr;

				// Point the pointer to first
				// element in array arr[]
				ptr = arr;

				// Traverse array using ptr
				for (int i = 0; i < N; i++) {

						// Print element at which
						// ptr points
						printf("%d ", ptr[0]);
						ptr++;
				}
			}"""
)

class PointerArithmeticTest16 extends StandardTest2("pointer comparison",
	"""
			void traverseArr(int* arr, int N, int M){
					int i, j;

					// Traverse rows of 2D matrix
					for (i = 0; i < N; i++) {

							// Traverse columns of 2D matrix
							for (j = 0; j < M; j++) {

									// Print the element
									printf("%d ", *((arr + i * M) + j));
							}
							printf("\n");
					}
			}

			int main(){

					int N = 3, M = 2;

					// A 2D array
					int arr[][2] = { { 1, 2 }, { 3, 4 }, { 5, 6 } };

					// Function Call
					traverseArr((int*)arr, N, M);
					return 0;
			}
			"""
)

class PointerArithmeticTest17 extends StandardTest2("pointer subtraction",
	"""
			void main(){
					int myNumbers[5] = {10, 20, 30, 40, 50};
					int *start = &myNumbers[1]; // points to 20
					int *end = &myNumbers[4];   // points to 50

					printf("%ld\n", end - start); // 3 elements apart
			}
			"""
)

class PointerArithmeticTest18 extends StandardTest2("using += 1 for pointer arithmetic",
	"""
			void main(){
				  int arr[] = {10, 20, 30, 40, 50, 60, 70};
					int *ptr = arr;
					ptr += 1;
					printf(" %d\n", *ptr);
		 			ptr += 2;
					printf(" %d\n", *ptr);
		 			ptr++;
					printf(" %d\n", *ptr);
		 			++ptr;
					printf(" %d\n", *ptr);
			}
			"""
)

class PointerArithmeticTest19 extends StandardTest2("using -= for pointer arithmetic",
	"""
			void main(){
					int arr[] = {10, 20, 30, 40, 50};
					int *ptr = &arr[4];
					ptr -= 2;
					printf(" %d\n", *ptr);
		 			ptr--;
					printf(" %d\n", *ptr);
		 			--ptr;
					printf(" %d\n", *ptr);
			}
			"""
)

class TwoDimAddressingCheck extends StandardTest2("2d array pointer arithmetic",
	"""
			void main() {
				int x[2][2] = {1,2,3,4};
				int *ptr = x[0];
				printf("%d\n", *ptr);
				ptr++;
				printf("%d\n", *ptr);
				ptr++;
				printf("%d\n", *ptr);
				ptr++;
				printf("%d\n", *ptr);
			}"""
)

class PointerTest22 extends StandardTest2("some incremental pointer arithmetic",
	"""
			void main() {
				char str[] = "Hello!\n";
				char *x = str;
				printf("%s", x);
				x++;
				printf("%s", x);
				x++;
				x++;
				printf("%s", x);
				x--;
				printf("%s", x);
			}"""
)

class PointerTest5 extends StandardTest2("some basic pointer arithmetic/indexing",
	"""
			void main() {
				unsigned char *str = calloc(12,1);
				memcpy(str, "Hello!\n", 6);
				char *x = str + 2;
				char y = str[2];
				printf("%d\n", *x == y);
				printf("%s\n", x);
				*x++;
//        str++;
//        str++;
//        ++str;
//        str--;
				printf("%s\n", str);
				printf("%s\n", x);
			}"""
)

class PointerTest6 extends StandardTest2("some basic pointer arithmetic",
	"""
			void main() {
				char str[] = "Hello!\n";
				char *x = str + 1;
				printf("%s\n", x);
				*x++;
				printf("%s\n", x);
			}"""
)

class PointerTest7 extends StandardTest2("some basic pointer arithmetic 2",
	"""
		void main() {
			char str[] = "Hello!\n";
			char *x = str;

			switch (x++[0]) {
				case 'H': printf("H\n"); break;
				case 'e': printf("e\n"); break;
				case 'l': printf("l\n"); break;
				case 'o': printf("o\n"); break;
			}

			printf("DONE\n");
		}"""
)
