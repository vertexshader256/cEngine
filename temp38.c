

      int x = 45;

      void test()
      {
        static int x = 10;
        x++;
        printf("%d\n", x);
      }

      void main()
      {
        test();
        test();
        test();
      }
      