package scala.c.engine

import better.files.File

class LinkedListTest extends StandardTest {
  "Linked list test" should "print the correct results" in {
    val code = """
        #include <stdlib.h>
        #include <stdbool.h>
        
        struct node {
           int data;
           int key;
           struct node *next;
        };
        
        struct node *head = NULL;
        struct node *current = NULL;
        
        //display the list
        void printList() {
           struct node *ptr = head;
           
           //start from the beginning
           while(ptr != NULL) {
              printf("(%d,%d)\n", ptr->key, ptr->data);
              ptr = ptr->next;
           }
        	
        }
        
        //insert link at the first location
        void insertFirst(int key, int data) {

           //create a link
           struct node *link = (struct node*) malloc(sizeof(struct node));
        	
           link->key = key;
           link->data = data;

           //point it to old first node
           link->next = head;
        	
           //point first to new first node
           head = link;
        }
        
        //delete first item
        struct node* deleteFirst() {
        
           //save reference to first link
           struct node *tempLink = head;
        	
           //mark next to first link as first 
           head = head->next;
        	
           //return the deleted link
           return tempLink;
        }
        
        //is list empty
        bool isEmpty() {
           return head == NULL;
        }
        
        int length() {
           int length = 0;
           struct node *current;
        	
           for(current = head; current != NULL; current = current->next) {
              length++;
           }
        	
           return length;
        }
        
        //find a link with given key
        struct node* find(int key) {
        
           //start from the first link
           struct node* current = head;
        
           //if list is empty
           if(head == NULL) {
              return NULL;
           }
        
           //navigate through list
           while(current->key != key) {

              //if it is last node
              if(current->next == NULL) {
                 return NULL;
              } else {
                 //go to next link
                 current = current->next;
              }
           }      
        	
           //if data found, return the current Link
           return current;
        }
        
        //delete a link with given key
        struct node* delete(int key) {
        
           //start from the first link
           struct node* current = head;
           struct node* previous = NULL;
        	
           //if list is empty
           if(head == NULL) {
              return NULL;
           }
        
           //navigate through list
           while(current->key != key) {
        
              //if it is last node
              if(current->next == NULL) {
                 return NULL;
              } else {
                 //store reference to current link
                 previous = current;
                 //move to next link
                 current = current->next;
              }
           }
        
           //found a match, update the link
           if(current == head) {
              //change first to point to next link
              head = head->next;
           } else {
              //bypass the current link
              previous->next = current->next;
           }    
        	
           return current;
        }
        
        void sort() {
        
           int i, j, k, tempKey, tempData;
           struct node *current;
           struct node *next;
        	
           int size = length();
           k = size ;
        	
           for ( i = 0 ; i < size - 1 ; i++, k-- ) {
              current = head;
              next = head->next;
        		
              for ( j = 1 ; j < k ; j++ ) {   
        		
                 if ( current->data > next->data ) {
                    tempData = current->data;
                    current->data = next->data;
                    next->data = tempData;
        
                    tempKey = current->key;
                    current->key = next->key;
                    next->key = tempKey;
                 }
        			
                 current = current->next;
                 next = next->next;
              }
           }   
        }
        
        void reverse(struct node** head_ref) {
           struct node* prev   = NULL;
           struct node* current = *head_ref;
           struct node* next;

           while (current != NULL) {
              next  = current->next;
              current->next = prev;   
              prev = current;
              current = next;
           }
        	
           *head_ref = prev;
        }
        
        void main() {
           insertFirst(1,10);

           insertFirst(2,20);
           insertFirst(3,30);
           insertFirst(4,1);
           insertFirst(5,40);
           insertFirst(6,56); 

           printList();
        
           while(!isEmpty()) {            
              struct node *temp = deleteFirst();
              printf("(%d,%d)\n", temp->key, temp->data);
           }  
        	
           printList();
           insertFirst(1,10);
           insertFirst(2,20);
           insertFirst(3,30);
           insertFirst(4,1);
           insertFirst(5,40);
           insertFirst(6,56);
           
           printList();
        
           struct node *foundLink = find(4);
        	
           if(foundLink != NULL) {
              printf("(%d,%d)\n",foundLink->key,foundLink->data);
           } else {
              printf("Element not found.\n");
           }
        
           delete(4);

           printList();
           foundLink = find(4);
        	
           if(foundLink != NULL) {
              printf("(%d,%d)\n",foundLink->key,foundLink->data);
           } else {
              printf("Element not found.\n");
           }
        	
           sort();
        	
           printList();
        	
           reverse(&head);

           printList();
        }
      
      """
    checkResults(code)
  }
}

class ComplexTest extends StandardTest {
  "Order of operations test 3" should "print the correct results" in {
    val code = """

      double sq_root(double x)
      {
        double rt = 1, ort = 0;
        while(ort!=rt)
        {
          ort = rt;
          rt = ((x/rt) + rt) / 2;
        }
        return rt;
      }

      void main()
      {
        printf("square root of %f\n",sq_root(9.0));
        printf("square root of %f\n",sq_root(3.0));
        printf("square root of %f\n",sq_root(15.0));
        return 0;
      }
      """

    checkResults(code)
  }


  "Fast inverse squart root test" should "print the correct results" in {
    val code = """float Q_rsqrt( float number )
    {
    	long i;
    	float x2, y;
    	const float threehalfs = 1.5F;

    	x2 = number * 0.5F;
    	y  = number;
    	i  = * ( int * ) &y;                       // evil floating point bit level hacking
    	i  = 0x5f3759df - ( i >> 1 );               // what the fuck?
    	y  = * ( float * ) &i;
    	y  = y * ( threehalfs - ( x2 * y * y ) );   // 1st iteration

    	return y;
    }

    void main()
    {
      printf("%f\n", Q_rsqrt(9.0));
      return 0;
    }
    """

    checkResults(code)
  }



  "FNV1a test" should "print the correct results" in {
    val code = """

      const unsigned int Prime = 0x01000193; //   16777619
      const unsigned int Seed  = 0x811C9DC5; // 2166136261

      int fnv1a(unsigned char oneByte, int hash)
      {
        printf("%d %d\n", oneByte, hash);
        printf("%d\n", (oneByte ^ hash));
        return (oneByte ^ hash) * Prime;
      }

      void main()
      {
          printf("%d\n", fnv1a(10, Seed));
//          printf("%d\n", fnv1a(232, Seed));
//          printf("%d\n", fnv1a(110, Seed));
//          printf("%d\n", fnv1a(65, Seed));
      }
      """

    checkResults(code)
  }

  "Kolakoski sequence" should "print the correct results" in {
    val code = """
       #define TRUE 1
       #define FALSE 0

       typedef int bool;

       int next_in_cycle(int *c, int len, int index) {
           return c[index % len];
       }

       void kolakoski(int *c, int *s, int clen, int slen) {
           int i = 0, j, k = 0;
           while (TRUE) {
               s[i] = next_in_cycle(c, clen, k);
               if (s[k] > 1) {
                   for (j = 1; j < s[k]; ++j) {
                       if (++i == slen) return;
                       s[i] = s[i - 1];
                   }
               }
               if (++i == slen) return;
               k++;
           }
       }

       bool possible_kolakoski(int *s, int len) {
           int i, j = 0, prev = s[0], count = 1;
           int *rle = calloc(len, sizeof(int));
           bool result = TRUE;
           for (i = 1; i < len; ++i) {
               if (s[i] == prev) {
                   count++;
               }
               else {
                   rle[j++] = count;
                   count = 1;
                   prev = s[i];
               }
           }
           /* no point adding final 'count' to rle as we're not going to compare it anyway */
           for (i = 0; i < j; i++) {
               if (rle[i] != s[i]) {
                  result = FALSE;
                  break;
               }
           }
           free(rle);
           return result;
       }

       void print_array(int *a, int len) {
           int i;
           printf("[");
           for (i = 0; i < len; ++i) {
              printf("%d", a[i]);
              if (i < len - 1) printf(", ");
           }
           printf("]");
       }

       int main() {
           int i, clen, slen, *s;
           int c0[2] = {1, 2};
           int c1[2] = {2, 1};
           int c2[4] = {1, 3, 1, 2};
           int c3[4] = {1, 3, 2, 1};
           int *cs[4] = {c0, c1, c2, c3};
           bool p;
           int clens[4] = {2, 2, 4, 4};
           int slens[4] = {20, 20, 30, 30};
           for (i = 0; i < 4; ++i) {
               clen = clens[i];
               slen = slens[i];
               s = calloc(slen, sizeof(int));
               kolakoski(cs[i], s, clen, slen);
               printf("First %d members of the sequence generated by ", slen);
               print_array(cs[i], clen);
               printf(":\n");
               print_array(s, slen);
               printf("\n");
               p = possible_kolakoski(s, slen);
               printf("Possible Kolakoski sequence? %s\n\n", p ? "True" : "False");
               free(s);
           }
           return 0;
       }
      """

    checkResults(code)
  }

  "knapsack problem" should "print the correct results" in {
    val code = """

      typedef struct {
           char *name;
           int weight;
           int value;
           int count;
       } item_t;

       // too many entries made this real slow
       item_t items[] = {
           {"map",                      9,   150,   1},
           {"compass",                 13,    35,   1},
           {"water",                  153,   200,   2}
       };

       int n = sizeof (items) / sizeof (item_t);

       int *knapsack (int w) {
           int i, j, k, v, *mm, **m, *s;
           mm = calloc((n + 1) * (w + 1), sizeof (int));
           m = malloc((n + 1) * sizeof (int *));
           m[0] = mm;
           for (i = 1; i <= n; i++) {
               m[i] = &mm[i * (w + 1)];
               for (j = 0; j <= w; j++) {
                   m[i][j] = m[i - 1][j];
                   for (k = 1; k <= items[i - 1].count; k++) {
                       if (k * items[i - 1].weight > j) {
                           break;
                       }
                       v = m[i - 1][j - k * items[i - 1].weight] + k * items[i - 1].value;
                       if (v > m[i][j]) {
                           m[i][j] = v;
                       }
                   }
               }
           }
           s = calloc(n, sizeof (int));
           for (i = n, j = w; i > 0; i--) {
               int v = m[i][j];
               for (k = 0; v != m[i - 1][j] + k * items[i - 1].value; k++) {
                   s[i - 1]++;
                   j -= items[i - 1].weight;
               }
           }
           free(mm);
           free(m);
           return s;
       }

       int main () {
           int i, tc = 0, tw = 0, tv = 0, *s;
           s = knapsack(400);
           for (i = 0; i < n; i++) {
               if (s[i]) {
                   printf("%s %5d %5d %5d\n", items[i].name, s[i], s[i] * items[i].weight, s[i] * items[i].value);
                   tc += s[i];
                   tw += s[i] * items[i].weight;
                   tv += s[i] * items[i].value;
               }
           }
           printf("%-22s %5d %5d %5d\n", "count, weight, value:", tc, tw, tv);
           return 0;
       }
      """

    checkResults(code)
  }

  "Palendrome test" should "print the correct results" in {
    val code = """

      int palindrome(const char *s)
      {
         const char *t; /* t is a pointer that traverses backwards from the end */
         for (t = s; *t != '\0'; t++) ; t--; /* set t to point to last character */

         while (s < t)
         {
           if ( *s++ != *t-- ) return 0; 
         }
         return 1;
      }
  
      void main()
      {
        char *test = "test";
        char *test2 = "acccbbbbbbccca";
        printf("%d\n", palindrome(test));
        printf("%d\n", palindrome(test2));
        return 0;
      }
      """

    checkResults(code)
  }  
}

class CaesarCipherTest extends StandardTest {
  "Caesar cipher" should "print the correct results" in {
    val code = """

      #define caesar(x) rot(13, x)
      #define decaesar(x) rot(13, x)
      #define decrypt_rot(x, y) rot((26-x), y)
       
      void rot(int c, char *str)
      {
      	int l = strlen(str);
      	const char *alpha[2] = { "abcdefghijklmnopqrstuvwxyz", "ABCDEFGHIJKLMNOPQRSTUVWXYZ"};
       
      	int i;
      	for (i = 0; i < l; i++)
      	{
      		if (!isalpha(str[i]))
      			continue;

      		str[i] = alpha[isupper(str[i])][((int)(tolower(str[i])-'a')+c)%26];		
      	}
      }
          
      int main()
      {
      	char str[] = "This is a top secret text message!";
       
      	printf("Original: %s\n", str);
      	caesar(str);
      	printf("Encrypted: %s\n", str);
      	decaesar(str);
      	printf("Decrypted: %s\n", str);
       
      	return 0;
      }
      """

    checkResults(code)
  }  
}
  
 class DJB2Test extends StandardTest {
    "DJB2 test" should "print the correct results" in {
      val code = """
  
        unsigned long
        djb2(unsigned char *str)
        {
            unsigned long hash = 5381;
            int c;
        
            while (c = *str++) {
                hash = ((hash << 5) + hash) + c; /* hash * 33 + c */
            }
        
            return hash;
        }
    
        void main()
        {
          char test[] = "TestString";
          printf("djb2 result %d\n", djb2(test));
          return 0;
        }
        """
  
      checkResults(code)
    }
 }

class DuffsTest extends StandardTest {
  "Duff device test" should "print the correct results" in {
    val code = """

        send(short *to, short *from, int count)
        {
         	int n=(count+7)/8;
         	switch(count%8){
         	case 0:	do{	*to++ = *from++;
         	case 7:		*to++ = *from++;
         	case 6:		*to++ = *from++;
         	case 5:		*to++ = *from++;
         	case 4:		*to++ = *from++;
         	case 3:		*to++ = *from++;
         	case 2:		*to++ = *from++;
         	case 1:		*to++ = *from++;
         		}while( --n>0);
         	}
        }

        void main()
        {
          int i = 0;
          int j = 0;
          for (j = 0; j < 10; j++) {
            short test1[10] = {1,2,3,4,5,6,7,8,9,10};
            short test2[10] = {0,0,0,0,0,0,0,0,0,0};
            send(test2, test1, j);
            for (i = 0; i < 10; i++) {
               printf("%d\n", test1[i]);
            }
            for (i = 0; i < 10; i++) {
              printf("%d\n", test2[i]);
            }
          }
          return 0;
        }
        """

    checkResults(code)
  }


}

class CarmichaelTest extends StandardTest {
  "Carmichael test" should "print the correct results" in {
    val code = """
    #include <stdio.h>

      /* C's % operator actually calculates the remainder of a / b so we need a
      * small adjustment so it works as expected for negative values */
      #define mod(n,m) ((((n) % (m)) + (m)) % (m))

      int is_prime(unsigned int n)
      {
      if (n <= 3) {
        return n > 1;
      }
      else if (!(n % 2) || !(n % 3)) {
        return 0;
      }
      else {
        unsigned int i;
        for (i = 5; i*i <= n; i += 6)
        if (!(n % i) || !(n % (i + 2)))
          return 0;
        return 1;
      }
      }

      void carmichael3(int p1)
      {
      if (!is_prime(p1)) return;

      int h3, d, p2, p3;
    for (h3 = 1; h3 < p1; ++h3) {
      for (d = 1; d < h3 + p1; ++d) {
        if ((h3 + p1)*(p1 - 1) % d == 0 && mod(-p1 * p1, h3) == d % h3) {
          p2 = 1 + ((p1 - 1) * (h3 + p1)/d);
          if (!is_prime(p2)) continue;
          p3 = 1 + (p1 * p2 / h3);
          if (!is_prime(p3) || (p2 * p3) % (p1 - 1) != 1) continue;
          printf("%d %d %d\n", p1, p2, p3);
        }
      }
    }
    }

    int main(void)
    {
    int p1;
    for (p1 = 2; p1 < 62; ++p1)
    carmichael3(p1);
    return 0;
    }"""

    checkResults(code)
  }
}

class PerniciousTest extends StandardTest {
  "Pernicious test" should "print the correct results" in {
    val code = """
        #include <stdio.h>

        typedef unsigned uint;
        uint is_pern(uint n)
        {
          uint c = 2693408940u; // int with all prime-th bits set
          while (n) c >>= 1, n &= (n - 1); // take out lowerest set bit one by one
          return c & 1;
        }

        int main(void)
        {
          uint i, c;
          for (i = c = 0; c < 25; i++)
          if (is_pern(i))
            printf("%u ", i), ++c;
          putchar('\n');

          for (i = 888888877u; i <= 888888888u; i++)
          if (is_pern(i))
            printf("%u ", i);
          putchar('\n');

          return 0;
        }"""

    checkResults(code, false)
  }
}

class CholskeyTest extends StandardTest {
  "Cholskey decomp test" should "print the correct results" in {
    val code =
      """
      #include <stdio.h>
      #include <stdlib.h>
      #include <math.h>

      double *cholesky(double *A, int n) {
        double *L = (double*)calloc(n * n, sizeof(double));
        if (L == NULL)
          exit(EXIT_FAILURE);

         int i = 0;
         int j = 0;
         int k = 0;

        for (i = 0; i < n; i++) {
          for (j = 0; j < (i+1); j++) {
            double s = 0;
            for (k = 0; k < j; k++) {
              s += L[i * n + k] * L[j * n + k];
            }
            L[i * n + j] = (i == j) ?
              sqrt(A[i * n + i] - s) :
              (1.0 / L[j * n + j] * (A[i * n + j] - s));
          }
        }

        return L;
      }

      void show_matrix(double *A, int n) {
        int i = 0;
        int j = 0;
        for (i = 0; i < n; i++) {
          for (j = 0; j < n; j++) {
            printf("%2.5f ", A[i * n + j]);
          }
          printf("\n");
        }
      }

      int main() {
        int n = 3;
        double m1[] = {25, 15, -5,
          15, 18,  0,
          -5,  0, 11};
        double *c1 = cholesky(m1, n);
        show_matrix(c1, n);
        printf("\n");
        free(c1);

        n = 4;
        double m2[] = {18, 22,  54,  42,
          22, 70,  86,  62,
          54, 86, 174, 134,
          42, 62, 134, 106};
        double *c2 = cholesky(m2, n);
        show_matrix(c2, n);
        free(c2);

        return 0;
      }"""

    checkResults(code)
  }
}

class AmicablePairsTest extends StandardTest {
  "Amicable Pairs test" should "print the correct results" in {
    val code =
      """
     #include <stdio.h>
     #include <stdlib.h>

     typedef unsigned int uint;

     int main(int argc, char **argv)
     {
       uint top = atoi(argv[1]);
       uint *divsum = malloc((top + 1) * sizeof(*divsum));
       uint pows[32] = {1, 0};

       for (uint i = 0; i <= top; i++) divsum[i] = 1;

       // sieve
       // only sieve within lower half , the modification starts at 2*p
       for (uint p = 2; p+p <= top; p++) {
         if (divsum[p] > 1) {
           divsum[p] -= p;// subtract number itself from divisor sum ('proper')
           continue;}     // p not prime

         uint x; // highest power of p we need
         //checking x <= top/y instead of x*y <= top to avoid overflow
         for (x = 1; pows[x - 1] <= top/p; x++)
           pows[x] = p*pows[x - 1];

         //counter where n is not a*p with a = ?*p, useful for most p.
         //think of p>31 seldom divisions or p>sqrt(top) than no division is needed
         //n = 2*p, so the prime itself is left unchanged => k=p-1
         uint k= p-1;
         for (uint n = p+p; n <= top; n += p) {
           uint s=1+pows[1];
           k--;
           // search the right power only if needed
           if ( k==0) {
             for (uint i = 2; i < x && !(n%pows[i]); s += pows[i++]);
             k = p; }
           divsum[n] *= s;
         }
       }

       //now correct the upper half
       for (uint p = (top >> 1)+1; p <= top; p++) {
         if (divsum[p] > 1){
           divsum[p] -= p;}
       }

       uint cnt = 0;
       for (uint a = 1; a <= top; a++) {
         uint b = divsum[a];
         if (b > a && b <= top && divsum[b] == a){
           printf("%u %u\n", a, b);
           cnt++;}
       }
       printf("\nTop %u count : %u\n",top,cnt);
       return 0;
      }"""

    checkResults(code, args = List("1500"))
  }
}

class SimpsonIntegrationTest extends StandardTest {
  "Simpson Integration test" should "print the correct results" in {
    val code =
      """
     #include <stdio.h>
     #include <math.h>

     typedef struct { double m; double fm; double simp; } triple;

     /* "structured" adaptive version, translated from Racket */
     triple _quad_simpsons_mem(double (*f)(double), double a, double fa, double b, double fb) {
         // Evaluates Simpson's Rule, also returning m and f(m) to reuse.
         double m = (a + b) / 2;
         double fm = f(m);
         double simp = fabs(b - a) / 6 * (fa + 4*fm + fb);
         triple t = {m, fm, simp};
         return t;
     }

     double _quad_asr(double (*f)(double), double a, double fa, double b, double fb, double eps, double whole, double m, double fm) {
         // Efficient recursive implementation of adaptive Simpson's rule.
         // Function values at the start, middle, end of the intervals are retained.
         triple lt = _quad_simpsons_mem(f, a, fa, m, fm);
         triple rt = _quad_simpsons_mem(f, m, fm, b, fb);
         double delta = lt.simp + rt.simp - whole;
         if (fabs(delta) <= eps * 15) return lt.simp + rt.simp + delta/15;
         return _quad_asr(f, a, fa, m, fm, eps/2, lt.simp, lt.m, lt.fm) +
                _quad_asr(f, m, fm, b, fb, eps/2, rt.simp, rt.m, rt.fm);
     }

     double quad_asr(double (*f)(double), double a, double b, double eps) {
         // Integrate f from a to b using ASR with max error of eps.
         double fa = f(a);
         double fb = f(b);
         triple t = _quad_simpsons_mem(f, a, fa, b, fb);
         return _quad_asr(f, a, fa, b, fb, eps, t.simp, t.m, t.fm);
     }

     int main(){
         double a = 0.0, b = 1.0;
         double sinx = quad_asr(sin, a, b, 1e-09);
         printf("Simpson's integration of sine from %f to %f = %f\n", a, b, sinx);
         return 0;
     }
    """

    checkResults(code)
  }
}
