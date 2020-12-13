#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>

void swap(int* xp, int* yp) {
  int temp = *xp;
  *xp = *yp;
  *yp = temp;
}

int main()
{
  FILE *input = fopen("10.input", "r");
  if( NULL == input ) {
    printf("File not found\n");
    exit(EXIT_FAILURE);
  }

  char * line = "\n";
  size_t len = 0;

  int MAX_LINES = 1000;
  int adapters[MAX_LINES];
  for(int i=0; i<MAX_LINES; i++) {
    adapters[i] = 0;
  }
  int i=0;
  int num_rows=0;
  adapters[num_rows++] = 0;
  while(-1 != getline(&line, &len, input)) {
    int num = strtol(line, NULL, 10);
    adapters[num_rows++] = num;
  }
  for(int i=0; i < num_rows; i++) {
    int min_idx=i;
    for(int j=i+1; j < num_rows; j++) {
      if (adapters[j] < adapters[min_idx]) {
        min_idx = j;
      }
    }
    swap(&adapters[min_idx], &adapters[i]);
  }
  adapters[num_rows++] = adapters[num_rows-1]+3;

  unsigned long wayz[num_rows+1];
  wayz[0] = 1;
  for(int i=1; i<num_rows; i++) { wayz[i] = 0; }

  int hops[3] = {0,0,0};
  for(int i=0; i<num_rows-1; i++) {
    if(adapters[i]+1 == adapters[i+1]) {
      hops[0]++;
    } else if(adapters[i]+2 == adapters[i+1]) {
      hops[1]++;
    } else if(adapters[i]+3 == adapters[i+1]) {
      hops[2]++;
    }
    for(int j=i+1; j<num_rows; j++) {
      if(adapters[j] <= adapters[i]+3) {
        wayz[j] += wayz[i];
      }
    }
  }
  printf("part 1: %d\n", hops[0]*hops[2]);
  printf("part 2: %lu\n", wayz[num_rows-1]);

  fclose(input);

  exit(EXIT_SUCCESS);
}
