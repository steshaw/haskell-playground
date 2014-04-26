#include <stdio.h>
#include "foo_stub.h"

int main(int argc, char *argv[]) {
  hs_init(&argc, &argv);

  char *word = comma("foo bar");
  printf("%s\n", word);

  hs_exit();
  return 0;
}
