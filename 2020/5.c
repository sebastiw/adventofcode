#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main()
{
  FILE *input = fopen("5.input", "r");
  if( NULL == input ) {
    exit(EXIT_FAILURE);
  }

  char * line = NULL;
  size_t len = 0;

  int highest_id = 0;
  int ids[1024];
  for(int i = 0; i < 1024; i++) {
    ids[i] = -1;
  }
  while(-1 != getline(&line, &len, input)) {
    int row = 0;
    int rows = 128;
    for(int i = 0; i < 7; i++) {
      rows /= 2;
      if('B' == line[i]) {
        row += rows;
      }
    }
    int col = 0;
    int cols = 8;
    for(int i = 7; i<10; i++) {
      cols /= 2;
      if('R' == line[i]) {
        col += cols;
      }
    }
    int id = row*8+col;
    ids[id] = 1;
    if(id > highest_id) {
      highest_id = id;
    }
  }
  printf("Highest seat id - part 1: %d\n", highest_id);
  for(int i = 1; i < 1023; i++) {
    if(ids[i-1] == 1 && ids[i] == -1 && ids[i+1] == 1) {
      printf("missing id - part 2: %d\n", i);
    }
  }

  fclose(input);

  exit(EXIT_SUCCESS);
}
