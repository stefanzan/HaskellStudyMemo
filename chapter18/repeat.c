#include <stdio.h>

int repeat(int *func(int, int), int a) {
  func(a, a+1);
  return repeat(func, a+1);
}

int f (int a , int b){
  printf("%d\n", a);
  return a + b;
}


int main(int argc, char const *argv[])
{
  /* code */
  repeat(f, 1);
  return 0;
}