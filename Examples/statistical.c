﻿int[1000] A;
{int average; int max; int std; int size} S;
int i;
S.size := 1000;
while (S.size > i) {
    int ele;
    ele := A[i];
    S.average := S.average + ele;
    if(S.max < ele)
    {
        S.max := ele;
    }
    S.std := S.std + ele*ele;
    i := i + 1;
}
S.std := (S.std - (S.average*S.average)/S.size)/(S.size-1);
S.average := S.average/S.size;
write S.average;
write S.max;
write S.std;