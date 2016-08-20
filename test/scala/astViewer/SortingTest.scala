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