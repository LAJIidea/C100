void assert(int expected, int actual);

int sum(int n) {
    int res = 0, i = 0;
    for (i=0;i<=n;i=i+1) {
        res = res + i;
    }
    return res;
}

int fib(int n) {
    if (n <= 1) {
        return 1;
    }
    return fib(n-1) + fib(n-2);
}

int main() {
    assert(1, sum(1));
    return 0;
}