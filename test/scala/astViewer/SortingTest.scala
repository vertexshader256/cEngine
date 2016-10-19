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

//class InsertionSort extends StandardTest {
//  "insertion sort test 1" should "print the correct results" in {
//      val code = """
//        void insertion_sort(int *a, int n) {
//          int i = 0;
//        	for(i = 1; i < n; ++i) {
//        		int tmp = a[i];
//        		int j = i;
//        		while(j > 0 && tmp < a[j - 1]) {
//        			a[j] = a[j - 1];
//        			--j;
//        		}
//        		a[j] = tmp;
//        	}
//        }
//         
//        int main () {
//            int a[] = {4, 65, 2, -31, 0, 99, 2, 83, 782, 1};
//            int n = sizeof a / sizeof a[0];
//            int i;
//            for (i = 0; i < n; i++)
//                printf("%d\n", a[i]);
//            //insertion_sort(a, n);
//        }
//        """
//      checkResults(code)
//    }
//  
//}

//class QuickSort extends StandardTest {
//    "quick sort test 1" should "print the correct results" in {
//      // http://rosettacode.org/wiki/Sorting_algorithms/Quicksort#C
//      val code = """
//        void quick_sort (int *a, int n) {
//            int i, j, p, t;
//            if (n < 2)
//                return;
//            p = a[n / 2];
//            for (i = 0, j = n - 1;; i++, j--) {
//                while (a[i] < p)
//                    i++;
//                while (p < a[j])
//                    j--;
//                if (i >= j)
//                    break;
//                t = a[i];
//                a[i] = a[j];
//                a[j] = t;
//            }
//            quick_sort(a, i);
//            quick_sort(a + i, n - i);
//        }
//         
//        int main (void) {
//            int a[] = {4, 65, 2, -31, 0, 99, 2, 83, 782, 1};
//            int n = sizeof a / sizeof a[0];
//            int i;
//            for (i = 0; i < n; i++)
//                printf("%d%s", a[i], i == n - 1 ? "\n" : " ");
//            quick_sort(a, n);
//            for (i = 0; i < n; i++)
//                printf("%d%s", a[i], i == n - 1 ? "\n" : " ");
//            return 0;
//        }
//        """
//      checkResults(code)
//  }
//  }