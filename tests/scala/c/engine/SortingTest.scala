package c.engine

  class RadixSort extends StandardTest {
    "radix sort test 1" should "print the correct results" in {
      val code = """
        //#include <stdio.h>
        #include <limits.h>
        //#include <stdlib.h>
         
        typedef unsigned uint;
        #define swap(a, b) { tmp = a; a = b; b = tmp; }
        #define each(i, x) for (i = 0; i < x; i++)
         
        /* sort unsigned ints */
        static void rad_sort_u(uint *from, uint *to, uint bit)
        {

        	if (!bit || to < from + 1) return;
         
        	uint *ll = from, *rr = to - 1, tmp;
//        	while (1) {
//        		/* find left most with bit, and right most without bit, swap */
//        		while (ll < rr && !(*ll & bit)) ll++;
//        		while (ll < rr &&  (*rr & bit)) rr--;
//        		if (ll >= rr) break;
//        		swap(*ll, *rr);
//        	}
        	
        	
         
        	if (!(bit & *ll) && ll < to) ll++;
        	bit >>= 1;
        	
        	
         
        	//rad_sort_u(from, ll, bit);
        	//rad_sort_u(ll, to, bit);
        }
         
        /* sort signed ints: flip highest bit, sort as unsigned, flip back */
        static void radix_sort(int *a, const int len)
        {
        	int i;
        	uint *x = (uint*) a;
         
        	each(i, len) x[i] ^= INT_MIN;
        	rad_sort_u(x, x + len, INT_MIN);
        	each(i, len) x[i] ^= INT_MIN;
        }
         
        static inline void radix_sort_unsigned(uint *a, const int len)
        {
        	rad_sort_u(a, a + len, (uint)INT_MIN);
        }
         
        int main(void)
        {
        	int len = 16, i;
        	int x[16] = {64, 285, 27, 501, 348, -293, 487, 542, 1645, 4, 8523, 7625, 184, 5792, 45803, 274};
         
        	radix_sort(x, len);
         
        	each(i, len) printf("%d\n", x[i]);
         
        	return 0;
        }
        """
      checkResults(code)
    }
  }

  class BubbleSort extends StandardTest {
    "bubble sort test 1" should "print the correct results" in {
      val code = """
        int main()
        {
          int array[5] = {7,4,0,2,6}, n = 5, c, d, swap;
         
          for (c = 0 ; c < ( n - 1 ); c++)
          {
            for (d = 0 ; d < n - c - 1; d++)
            {
              if (array[d] > array[d+1]) /* For decreasing order use < */
              {
                swap       = array[d];
                array[d]   = array[d+1];
                array[d+1] = swap;
              }
            }
          }
  
          for ( c = 0 ; c < n ; c++ )
             printf("%d\n", array[c]);
         
          return 0;
        }
        """
      checkResults(code)
    }
  }

class InsertionSort extends StandardTest {
  "insertion sort test 1" should "print the correct results" in {
      val code = """
        void insertion_sort(int *a, int n) {
          int i = 0;
        	for(i = 1; i < n; ++i) {
        		int tmp = a[i];
        		int j = i;
        		while(j > 0 && tmp < a[j - 1]) {
        			a[j] = a[j - 1];
        			--j;
        		}
        		a[j] = tmp;
        	}
        }
         
        int main () {
            int a[] = {4, 65, 2, -31, 0, 99, 2, 83, 782, 1};
            int n = sizeof a / sizeof a[0];
            int i;
            for (i = 0; i < n; i++)
                printf("%d\n", a[i]);
            insertion_sort(a, n);
        }
        """
      checkResults(code)
    }
  
}

//class StrandSort extends StandardTest {
//  "strand sort test 1" should "print the correct results" in {
//    // http://rosettacode.org/wiki/Sorting_algorithms/Quicksort#C
//    val code = """
//        #include <stdio.h>
//
//        typedef struct node_t *node, node_t;
//        struct node_t { int v; node next; };
//        typedef struct { node head, tail; } slist;
//
//        void push(slist *l, node e) {
//          if (!l->head) l->head = e;
//          if (l->tail)  l->tail->next = e;
//          l->tail = e;
//        }
//
//        node removehead(slist *l) {
//          node e = l->head;
//          if (e) {
//            l->head = e->next;
//            e->next = 0;
//          }
//          return e;
//        }
//
//        void join(slist *a, slist *b) {
//          push(a, b->head);
//          a->tail = b->tail;
//        }
//
//        void merge(slist *a, slist *b) {
//          slist r = {0};
//          while (a->head && b->head)
//            push(&r, removehead(a->head->v <= b->head->v ? a : b));
//
//          join(&r, a->head ? a : b);
//          *a = r;
//          b->head = b->tail = 0;
//        }
//
//        void sort(int *ar, int len)
//        {
//          node_t all[len];
//
//          // array to list
//          for (int i = 0; i < len; i++)
//            all[i].v = ar[i], all[i].next = i < len - 1 ? all + i + 1 : 0;
//
//          slist list = {all, all + len - 1}, rem, strand = {0},  res = {0};
//
//          for (node e = 0; list.head; list = rem) {
//            rem.head = rem.tail = 0;
//            while ((e = removehead(&list)))
//              push((!strand.head || e->v >= strand.tail->v) ? &strand : &rem, e);
//
//            merge(&res, &strand);
//          }
//
//          // list to array
//          for (int i = 0; res.head; i++, res.head = res.head->next)
//            ar[i] = res.head->v;
//        }
//
//        void show(const char *title, int *x, int len)
//        {
//          printf("%s ", title);
//          for (int i = 0; i < len; i++)
//            printf("%3d ", x[i]);
//          putchar('\n');
//        }
//
//        int main(void)
//        {
//          int x[] = {-2,0,-2,5,5,3,-1,-3,5,5,0,2,-4,4,2};
//        #	define SIZE sizeof(x)/sizeof(int)
//
//          show("before sort:", x, SIZE);
//          sort(x, sizeof(x)/sizeof(int));
//          show("after sort: ", x, SIZE);
//
//          return 0;
//        }
//        """
//    checkResults(code)
//  }
//}

class QuickSort extends StandardTest {
    "quick sort test 1" should "print the correct results" in {
      // http://rosettacode.org/wiki/Sorting_algorithms/Quicksort#C
      val code = """
        void quick_sort (int *a, int n) {
            int i, j, p, t;
            if (n < 2)
                return;
            p = a[n / 2];
            
            for (i = 0, j = n - 1;; i++, j--) {
                while (a[i] < p)
                    i++;
                while (p < a[j])
                    j--;
                if (i >= j)
                    break;
                t = a[i];
                a[i] = a[j];
                a[j] = t;
            }
            quick_sort(a, i);
            quick_sort(a + i, n - i);
        }
         
        int main (void) {
            int a[] = {4, 65, 2, -31, 0, 99, 2, 83, 782, 1};
            int n = sizeof a / sizeof a[0];
            int i;
            for (i = 0; i < n; i++)
                printf("%d\n", a[i]);
            quick_sort(a, n);
            for (i = 0; i < n; i++)
                printf("%d\n", a[i]);
            return 0;
        }
        """
      checkResults(code)
  }
}

class ShellSort extends StandardTest {
    "shell sort test 1" should "print the correct results" in {
      // http://rosettacode.org/wiki/Sorting_algorithms/Shell_sort#C
      val code = """
        void bead_sort(int *a, int len)
        {
        	int i, j, max, sum;
        	unsigned char *beads;
        #	define BEAD(i, j) beads[i * max + j]
         
        	for (i = 1, max = a[0]; i < len; i++)
        		if (a[i] > max) max = a[i];
         
        	beads = calloc(1, max * len);
         
        	/* mark the beads */
        	for (i = 0; i < len; i++)
        		for (j = 0; j < a[i]; j++)
        			BEAD(i, j) = 1;
         
        	for (j = 0; j < max; j++) {
        		/* count how many beads are on each post */
        		for (sum = i = 0; i < len; i++) {
        			sum += BEAD(i, j);
        			BEAD(i, j) = 0;
        		}
        		/* mark bottom sum beads */
        		for (i = len - sum; i < len; i++) BEAD(i, j) = 1;
        	}
         
        	for (i = 0; i < len; i++) {
        		for (j = 0; j < max && BEAD(i, j); j++);
        		a[i] = j;
        	}
        	free(beads);
        }
         
        int main()
        {
        	int i, x[] = {5, 3, 1, 7, 4, 1, 1, 20};
        	int len = sizeof(x)/sizeof(x[0]);
         
        	bead_sort(x, len);
        	for (i = 0; i < len; i++)
        		printf("%d\n", x[i]);
         
        	return 0;
        }
        """
      checkResults(code)
  }
}

class BogoSort extends StandardTest {
    "bogo sort test 1" should "print the correct results" in {
      // http://rosettacode.org/wiki/Sorting_algorithms/Bogo_sort#C
      val code = """
        #include <stdbool.h>
        
        bool is_sorted(int *a, int n)
        {
          while ( --n >= 1 ) {
            if ( a[n] < a[n-1] ) return false;
          }
          return true;
        }
         
        void shuffle(int *a, int n)
        {
          int i, t, r;
          for(i=0; i < n; i++) {
            t = a[i];
            r = rand() % n;
            a[i] = a[r];
            a[r] = t;
          }
        }
         
        void bogosort(int *a, int n)
        {
          while ( !is_sorted(a, n) ) shuffle(a, n);
        }
         
        int main()
        {
          int numbers[] = { 1, 10, 9,  7, 3, 0 };
          int i;
         
          bogosort(numbers, 6);
          for (i=0; i < 6; i++) printf("%d\n", numbers[i]);
        }
        """
      checkResults(code)
  }
}

class BeadSort extends StandardTest {
  "bead sort test 1" should "print the correct results" in {
    // http://rosettacode.org/wiki/Sorting_algorithms/Circle_sort#C
    val code = """
        #include <stdio.h>
        #include <stdlib.h>

        void bead_sort(int *a, int len)
        {
        	int i, j, max, sum;
        	unsigned char *beads;
        #	define BEAD(i, j) beads[i * max + j]

        	for (i = 1, max = a[0]; i < len; i++)
        		if (a[i] > max) max = a[i];

        	beads = calloc(1, max * len);

        	/* mark the beads */
        	for (i = 0; i < len; i++)
        		for (j = 0; j < a[i]; j++)
        			BEAD(i, j) = 1;

        	for (j = 0; j < max; j++) {
        		/* count how many beads are on each post */
        		for (sum = i = 0; i < len; i++) {
        			sum += BEAD(i, j);
        			BEAD(i, j) = 0;
        		}
        		/* mark bottom sum beads */
        		for (i = len - sum; i < len; i++) BEAD(i, j) = 1;
        	}

        	for (i = 0; i < len; i++) {
        		for (j = 0; j < max && BEAD(i, j); j++);
        		a[i] = j;
        	}
        	free(beads);
        }

        int main()
        {
       	  int i, x[] = {5, 3, 1, 7, 4, 1, 1, 20};
          int len = sizeof(x)/sizeof(x[0]);

          bead_sort(x, len);
         	 for (i = 0; i < len; i++)
        		 printf("%d\n", x[i]);

       	  return 0;
       }
        """
    checkResults(code)
  }
}

class CircleSort extends StandardTest {
    "circle sort test 1" should "print the correct results" in {
      // http://rosettacode.org/wiki/Sorting_algorithms/Circle_sort#C
      val code = """
        int circle_sort_inner(int *start, int *end)
        {
        	int *p, *q, t, swapped;

        	if (start == end) 
        	  return 0;

        	// funny "||" on next line is for the center element of odd-lengthed array
        	for (swapped = 0, p = start, q = end; p<q || (p==q && ++q); p++, q--)
        		if (*p > *q) {
        			t = *p, *p = *q, *q = t, swapped = 1;
        		}
         
        	// q == p-1 at this point

        	return swapped | circle_sort_inner(start, q) | circle_sort_inner(p, end);
        }
         
        //helper function to show arrays before each call
        void circle_sort(int *x, int n)
        {
        	do {
        		int i;
        		for (i = 0; i < n; i++) printf("%d\n", x[i]);
        		
        	} while (circle_sort_inner(x, x + (n - 1)));
        }
         
        int main(void)
        {
        	int x[] = {5, -1, 101, -4, 0, 1, 8, 6, 2, 3};
        	circle_sort(x, sizeof(x) / sizeof(*x));
         
        	return 0;
        }
        """
      checkResults(code)
  }
}

class MergeSort extends StandardTest {
    "merge sort test 1" should "print the correct results" in {
      // http://quiz.geeksforgeeks.org/merge-sort/
      val code = """
        // Merges two subarrays of arr[].
        // First subarray is arr[l..m]
        // Second subarray is arr[m+1..r]
        void merge(int arr[], int l, int m, int r)
        {
            int i, j, k;
            int n1 = m - l + 1;
            int n2 =  r - m;
         
            /* create temp arrays */
            int L[n1], R[n2];
         
            /* Copy data to temp arrays L[] and R[] */
            for (i = 0; i < n1; i++)
                L[i] = arr[l + i];
            for (j = 0; j < n2; j++)
                R[j] = arr[m + 1+ j];
         
            /* Merge the temp arrays back into arr[l..r]*/
            i = 0; // Initial index of first subarray
            j = 0; // Initial index of second subarray
            k = l; // Initial index of merged subarray
            while (i < n1 && j < n2)
            {
                if (L[i] <= R[j])
                {
                    arr[k] = L[i];
                    i++;
                }
                else
                {
                    arr[k] = R[j];
                    j++;
                }
                k++;
            }
         
            /* Copy the remaining elements of L[], if there
               are any */
            while (i < n1)
            {
                arr[k] = L[i];
                i++;
                k++;
            }
         
            /* Copy the remaining elements of R[], if there
               are any */
            while (j < n2)
            {
                arr[k] = R[j];
                j++;
                k++;
            }
        }
         
        /* l is for left index and r is right index of the
           sub-array of arr to be sorted */
        void mergeSort(int arr[], int l, int r)
        {
            if (l < r)
            {
                // Same as (l+r)/2, but avoids overflow for
                // large l and h
                int m = l+(r-l)/2;
         
                // Sort first and second halves
                mergeSort(arr, l, m);
                mergeSort(arr, m+1, r);
         
                merge(arr, l, m, r);
            }
        }
        
        /* UTILITY FUNCTIONS */
        /* Function to print an array */
        void printArray(int A[], int size)
        {
            int i;
            for (i=0; i < size; i++)
                printf("%d\n", A[i]);
        }
        
        /* Driver program to test above functions */
        int main()
        {
            int arr[] = {12, 11, 13, 5, 6, 7};
            int arr_size = sizeof(arr)/sizeof(arr[0]);
         
            printArray(arr, arr_size);
         
            mergeSort(arr, 0, arr_size - 1);
         
            printArray(arr, arr_size);
            return 0;
        }
        """
      checkResults(code)
  }
}

class Heapsort extends StandardTest {
    "heapsort test 1" should "print the correct results" in {
      // https://rosettacode.org/wiki/Sorting_algorithms/Heapsort#C
      val code = """
        int max (int *a, int n, int i, int j, int k) {
            int m = i;
            if (j < n && a[j] > a[m]) {
                m = j;
            }
            if (k < n && a[k] > a[m]) {
                m = k;
            }
            return m;
        }
         
        void downheap (int *a, int n, int i) {
            while (1) {
                int j = max(a, n, i, 2 * i + 1, 2 * i + 2);
                if (j == i) {
                    break;
                }
                int t = a[i];
                a[i] = a[j];
                a[j] = t;
                i = j;
            }
        }
         
        void heapsort (int *a, int n) {
            int i;
            for (i = (n - 2) / 2; i >= 0; i--) {
                downheap(a, n, i);
            }
            for (i = 0; i < n; i++) {
                int t = a[n - i - 1];
                a[n - i - 1] = a[0];
                a[0] = t;
                downheap(a, n - i - 1, 0);
            }
        }
         
        int main () {
            int a[] = {4, 65, 2, -31, 0, 99, 2, 83, 782, 1};
            int n = sizeof a / sizeof a[0];
            int i;
            for (i = 0; i < n; i++)
                printf("%d\n", a[i]);
            heapsort(a, n);
            for (i = 0; i < n; i++)
                printf("%d\n", a[i]);
            return 0;
        }
        """
      checkResults(code)
  }
}