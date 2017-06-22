package c.engine

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
    checkResults(code, false)
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

    checkResults(code, false)
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
          printf("%d\n", fnv1a(232, Seed));
          printf("%d\n", fnv1a(110, Seed));
          printf("%d\n", fnv1a(65, Seed));
        return 0;
      }
      """

    checkResults(code, false)
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

    checkResults(code, false)
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

    checkResults(code, false)
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
  
      checkResults(code, false)
    }
 }

class DuffsTest extends StandardTest {
  "Duff device test" should "print the correct results" in {
    val code = """

       distangled(short *to, short *from, int count)
       {
           int n = (count + 7) / 8;
           switch (count % 8) {
               case 0: *to++ = *from++;
               case 7: *to++ = *from++;
               case 6: *to++ = *from++;
               case 5: *to++ = *from++;
               case 4: *to++ = *from++;
               case 3: *to++ = *from++;
               case 2: *to++ = *from++;
               case 1: *to++ = *from++;
           }
           while (--n > 0) {
               *to++ = *from++;
               *to++ = *from++;
               *to++ = *from++;
               *to++ = *from++;
               *to++ = *from++;
               *to++ = *from++;
               *to++ = *from++;
               *to++ = *from++;
           }
       }

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
            distangled(test2, test1, j);
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

    checkResults(code, false)
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

    checkResults(code, false)
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
  "Cholskey decomp test test" should "print the correct results" in {
    val code =
      """

      #include <stdio.h>
        #include <stdlib.h>
          #include <math.h>

            double *cholesky(double *A, int n) {
            double *L = (double*)calloc(n * n, sizeof(double));
            if (L == NULL)
              exit(EXIT_FAILURE);

            for (int i = 0; i < n; i++)
      for (int j = 0; j < (i+1); j++) {
        double s = 0;
        for (int k = 0; k < j; k++)
        s += L[i * n + k] * L[j * n + k];
        L[i * n + j] = (i == j) ?
          sqrt(A[i * n + i] - s) :
          (1.0 / L[j * n + j] * (A[i * n + j] - s));
      }

      return L;
      }

      void show_matrix(double *A, int n) {
      for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++)
        printf("%2.5f ", A[i * n + j]);
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

    checkResults(code, false)
  }
}
