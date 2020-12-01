int[42] A;
int i;
int j;
int t;
int n;
int distinct;

n := 42;
i := 1;
while (i < n) {
  j := i;
  while (j > 0 && (A[j-1] > A[j])) {
    t := A[j];
    A[j] := A[j-1];
    A[j-1] := t;
    j := j-1;
  }
  i := i+1;
}

distinct := 0;
i := 0;
while (i < n - 1) {
  if (A[i] != A[i+1]) {
    distinct := distinct + 1;
  }
}
write distinct;