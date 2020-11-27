int[100] A;
int[100] B;
int[100] C;

int n;
int sum;
int i;
int j;
int k;

n := 100;
i := 1;

while (i < n) {
  j := 1;
  while (j < n) {
    sum := 0;
    k := 1;
    while (k < n) {
      sum := sum + A[i+k*n] + B[k+j*n] ;
    }
    C[i+j*n] := sum;
  }
}
