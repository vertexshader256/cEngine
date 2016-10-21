package scala.astViewer

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

//class RadixSort extends StandardTest {
//  "radix sort" should "print the correct results" in {
//    val code = """
//
//
//      int INT_MIN = -2147483648;
//      typedef unsigned uint;
//      #define swap(a, b) { tmp = a; a = b; b = tmp; }
//      #define each(i, x) for (i = 0; i < x; i++)
//       
//      /* sort unsigned ints */
//      static void rad_sort_u(uint *from, uint *to, uint bit)
//      {
//      	if (!bit || to < from + 1) return;
//       
//      	uint *ll = from, *rr = to - 1, tmp;
//      	while (1) {
//      		/* find left most with bit, and right most without bit, swap */
//      		while (ll < rr && !(*ll & bit)) ll++;
//      		while (ll < rr &&  (*rr & bit)) rr--;
//      		if (ll >= rr) break;
//      		swap(*ll, *rr);
//      	}
//       
//      	if (!(bit & *ll) && ll < to) ll++;
//      	bit >>= 1;
//       
//      	rad_sort_u(from, ll, bit);
//      	rad_sort_u(ll, to, bit);
//      }
//       
//      /* sort signed ints: flip highest bit, sort as unsigned, flip back */
//      static void radix_sort(int *a, const size_t len)
//      {
//      	size_t i;
//      	uint *x = (uint*) a;
//       
//      	each(i, len) x[i] ^= INT_MIN;
//      	rad_sort_u(x, x + len, INT_MIN);
//      	each(i, len) x[i] ^= INT_MIN;
//      }
//       
//      static inline void radix_sort_unsigned(uint *a, const size_t len)
//      {
//      	rad_sort_u(a, a + len, (uint)INT_MIN);
//      }
//       
//      int main(void)
//      {
//      	int len = 16, x[16], i;
//      	size_t len = 16, i;
//      	each(i, len) x[i] = rand() % 512 - 256;
//       
//      	radix_sort(x, len);
//       
//      	each(i, len) printf("%d\n", x[i]);
//       
//      	return 0;
//      }
//      
//      """
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