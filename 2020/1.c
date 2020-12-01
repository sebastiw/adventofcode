#include <stdio.h>
#include <stdlib.h>

int loop_part_1();
int loop_part_2();

int main()
{
  FILE *input = fopen("1.input", "r");
  if( NULL == input ) {
    exit(EXIT_FAILURE);
  }

  char * line = NULL;
  size_t len = 0;

  int i = -1;
  int SIZE = 200;
  long num_array[SIZE];
  int found_2 = 1;
  int found_3 = 1;
  while(getline(&line, &len, input) != -1) {
    const long num = strtol(line, NULL, 10);
    num_array[++i] = num;
    if(1 == found_2) {
      found_2 = loop_part_1(num_array, i, num);
    }
    if(1 == found_3) {
      found_3 = loop_part_2(num_array, i, num);
    }
  }

  fclose(input);

  exit(EXIT_SUCCESS);
}


int loop_part_1(long previous[], int current_idx, long current) {
  for(int j=0; j<current_idx; j++) {
    if(previous[j]+current == 2020) {
      printf("part1: %d\n", previous[j]*current);
      return 0;
    }
  }
  return 1;
}

int loop_part_2(long previous[], int current_idx, long current) {
  for(int j=0; j<current_idx-1; j++) {
    for(int k=j+1; k<current_idx; k++) {
      if(previous[j]+previous[k]+current == 2020) {
        printf("part2: %d\n", previous[j]*previous[k]*current);
        return 0;
      }
    }
  }
  return 1;
}
