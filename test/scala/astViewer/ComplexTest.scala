package scala.astViewer

class LinkedListTest extends StandardTest {
  "Linked list test" should "print the correct results" in {
    val code = """
      //  #include <stdio.h>
      //  #include <string.h>
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
           
           printf("%d\n", head->next);
           
           insertFirst(2,20);
           insertFirst(3,30);
           insertFirst(4,1);
           insertFirst(5,40);
           insertFirst(6,56); 
           
           printf("%d\n", head->key);
           printf("%d\n", head->next->key);
           printf("%d\n", head->next->next->key);

           //print list
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
//        	
//           reverse(&head);

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
      printf("%f\n", Q_rsqrt(9.0F)); // TODO: Dont require 'F' at end
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

    checkResults(code)
  }  
  
//  "Palendrome test" should "print the correct results" in {
//    val code = """
//
//      int palindrome(const char *s)
//      {
//         const char *t; /* t is a pointer that traverses backwards from the end */
//         for (t = s; *t != '\0'; t++) ; t--; /* set t to point to last character */
//         while (s < t)
//         {
//           if ( *s++ != *t-- ) return 0; 
//         }
//         return 1;
//      }
//  
//      void main()
//      {
//        char *test = "test";
//        printf("%d\n", palindrome(test));
//        return 0;
//      }
//      """
//
//    checkResults(code)
//  }  
  
  
  
//  "Caesar cipher" should "print the correct results" in {
//    val code = """
//       
//      #define caesar(x) rot(13, x)
//      #define decaesar(x) rot(13, x)
//      #define decrypt_rot(x, y) rot((26-x), y)
//       
//      void rot(int c, char *str)
//      {
//      	int l = strlen(str);
//      	const char *alpha[2] = { "abcdefghijklmnopqrstuvwxyz", "ABCDEFGHIJKLMNOPQRSTUVWXYZ"};
//       
//      	int i;
//      	for (i = 0; i < l; i++)
//      	{
//      		if (!isalpha(str[i]))
//      			continue;
//       
//      		str[i] = alpha[isupper(str[i])][((int)(tolower(str[i])-'a')+c)%26];
//      	}
//      }
//       
//       
//      int main()
//      {
//      	char str[] = "This is a top secret text message!";
//       
//      	printf("Original: %s\n", str);
//      	caesar(str);
//      	printf("Encrypted: %s\n", str);
//      	decaesar(str);
//      	printf("Decrypted: %s\n", str);
//       
//      	return 0;
//      }
//      """
//
//    checkResults(code)
//  }  
  
//  "DJB2 test" should "print the correct results" in {
//    val code = """
//
//      unsigned long
//      djb2(unsigned char *str)
//      {
//          unsigned long hash = 5381;
//          int c;
//      
//          while (c = *str++)
//              hash = ((hash << 5) + hash) + c; /* hash * 33 + c */
//      
//          return hash;
//      }
//  
//      void main()
//      {
//        char test[] = "TestString";
//        printf("djb2 result %d\n", djb2(test));
//        return 0;
//      }
//      """
//
//    checkResults(code)
//  }
}