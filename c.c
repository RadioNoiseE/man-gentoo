#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "opencc/opencc.h"
#define E "malloc FL.\n"

struct k {
  char *m;
  long long s;
} a;

int main() {
  opencc_t c = opencc_open("t2s.json");

  a.m = NULL;
  a.s = 0;

  FILE *f;
  f = fopen("main.md", "r");

  long long i = 0;
  long h = 0;

  while ((h = fgetc(f)) != EOF) {
    if (i >= a.s) {
      a.s += 1234;
      a.m = realloc(a.m, a.s);
      if (a.m == NULL) {
        printf(E);
        return 1;
      }
    }
    a.m[i] = h;
    i++;
  }

  char *v = opencc_convert_utf8(c, a.m, strlen(a.m));
  printf("%s", v);
  opencc_convert_utf8_free(v);
  opencc_close(c);

  return 0;
}
