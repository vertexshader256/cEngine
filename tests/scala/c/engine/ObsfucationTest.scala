package scala.c.engine

class ObsfucationTest extends StandardTest {
  "obsfucation 1" should "print the correct results" in {
    val code = """

      #include <stdio.h>
 main() {
   long long P = 1,
             E = 2,
             T = 5,
             A = 61,
             L = 251,
             N = 3659,
             R = 271173410,
             G = 1479296389,
             //x[] = { G * R * E * E * T , P * L * A * N * E * T };
             x[] = { G * R * E * E * T , P * L * A * N * E * T };
     printf("%s\n", (char*)x);
 }"""

    checkResults(code)
  }



  "obsfucation 2" should "print the correct results" in {
    val code = """
   #define u unsigned char
   #define v while(*x)
   #define z(x) putchar(*x);
   #define y(x) ++*x;
   #define p(x) ++x;
   #define e(x) --*x;
   #define d(x) --x;
   #define w(x) x x
   main(){u *x=calloc(12,1);u *l=x;w(w(w(y(x))))w(y(x))v{p(x)w(w(y(x)))w(y(x))y(x)p
   (x)w(w(w(y(x))))w(y(x))p(x)w(y(x))y(x)p(x)w(w(w(y(x))))y(x)w(w(d(x)))e(x)}p(x)w(
   y(x))z(x)p(x)y(x)z(x)w(w(y(x)))w(y(x))y(x)w(z(x))w(y(x))y(x)z(x)p(x)w(y(x))z(x)p
   (x)w(e(x))e(x)z(x)w(d(x))z(x)w(y(x))y(x)z(x)w(w(e(x)))w(e(x))z(x)w(w(w(e(x))))z(
   x)p(x)y(x)z(x)free(l);}
   """

    checkResults(code)
  }

//  "obsfucation 3" should "print the correct results" in {
//    val code = """
//   int m=1711276033,N=1,t[1<<25]={2},a,*p,i,e=39717691,s,c,U=1;g(d,h)
//   {for(i=s;i<1<< 24;i*=2)d=d*1LL*d%m;for(p=t;p<t+N;p+=s)for(i=s,c=1;i;i--)a=p[s]*
//     (h?c:1LL)%m,p[s] =(m*1U+*p-a)*(h?1LL:c)%m,*p=
//     (a*1U+*p)%m,p++,c=c*1LL*d%m;}main(){while(e/=2){N*=2 ;U=U*1LL*(m+1)/2%m;
//     for(s=N;s/=2;)g(40,0);for(p=t;p<t+N;p++)*p=*p*1LL**p%m*U%m; for(s=1;
//     s<N;s*=2)g(983983719,1);for(a=0,p=t;p<t+N;)a+=*p<<(e&1),*p++=a%10,a/=10;
//   }while(!*--p);for(t[0]--;p>=t;)putchar(48+*p--);}
//   """
//
//    checkResults(code)
//  }
}