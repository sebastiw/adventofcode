#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>

int main()
{
  FILE *input = fopen("9.input", "r");
  if( NULL == input ) {
    printf("File not found\n");
    exit(EXIT_FAILURE);
  }

  char * line = "\n";
  size_t len = 0;

  int PREAMBLE_LENGTH = 25;
  int preamble[PREAMBLE_LENGTH];
  int values[1001];
  for(int i=0; i<PREAMBLE_LENGTH; i++) {
    getline(&line, &len, input);
    preamble[i] = strtol(line, NULL, 10);
    values[i] = preamble[i];
  }

  int num;
  int found;
  int line_no = PREAMBLE_LENGTH;
  while(-1 != getline(&line, &len, input)) {
    num = strtol(line, NULL, 10);
    values[line_no] = num;
    found = 0;
    for(int i=0; i<PREAMBLE_LENGTH-1; i++) {
      for(int j=i+1; j<PREAMBLE_LENGTH; j++) {
        if((preamble[i]+preamble[j]) == num) {
          found = 1;
          break;
        }
      }
    }
    if(!found) {
      printf("part 1: %d\n", line_no, num);
      break;
    } else {
      preamble[line_no%PREAMBLE_LENGTH] = num;
    }
    line_no++;
  }

  int sum = 0; int i; int j;
  for(i=0; sum != num && i<line_no; i++) {
    sum = values[i];
    for(j=i+1; sum<num; j++) {
      sum += values[j];
    }
  }
  printf("i: %d\nj: %d\n", i, j);
  int smallest = INT_MAX; int largest = INT_MIN;
  for(; i<j; i++) {
    if(values[i] < smallest) {
      smallest = values[i];
    }
    if(values[i] > largest) {
      largest = values[i];
    }
  }
  printf("part2: %d\n", smallest+largest);

  fclose(input);

  exit(EXIT_SUCCESS);
}
