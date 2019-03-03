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
//         #define P(X)j=write(1,X,1)
//       #define C 39
//       int M[5000]={2},*u=M,N[5000],R=22,a[4],l[]={0,-1,C-1,-1},m[]={1,-C,-1,C},*b=N,
//       *d=N,c,e,f,g,i,j,k,s;main(){for(M[i=C*R-1]=24;f|d>=b;){c=M[g=i];i=e;for(s=f=0;
//       s<4;s++)if((k=m[s]+g)>=0&&k<C*R&&l[s]!=k%C&&(!M[k]||!j&&c>=16!=M[k]>=16))a[f++
//       ]=s;if(f){f=M[e=m[s=a[rand()/(1+2147483647/f)]]+g];j=j<f?f:j;f+=c&-16*!j;M[g]=
//       c|1<<s;M[*d++=e]=f|1<<(s+2)%4;}else e=d>b++?b[-1]:e;}P(" ");for(s=C;--s;P("_")
//       )P(" ");for(;P("\n"),R--;P("|"))for(e=C;e--;P("_ "+(*u++/8)%2))P("| "+(*u/4)%2
//       );}
//   """
//
//    checkResults(code)
//  }
}