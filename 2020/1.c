#include <stdio.h>
#include <stdlib.h>

int part_1();
int part_2();

int main()
{
  part_1();
  part_2();

  exit(EXIT_SUCCESS);
}


int part_1()
{
  FILE *input = fopen("1.input", "r");
  if( NULL == input ) {
    exit(EXIT_FAILURE);
  }

  char * line = NULL;
  size_t len = 0;

  int i = 0;
  int SIZE = 200;
  long num_array[SIZE];
  while(getline(&line, &len, input) != -1) {
    const long num = strtol(line, NULL, 10);
    num_array[i++] = num;
    for(int j=0; j<i-1; j++) {
      if(num_array[j]+num == 2020) {
        printf("part1: %d\n", num_array[j]*num);
        break;
      }
    }
  }

  fclose(input);
}

int part_2()
{
  FILE *input = fopen("1.input", "r");
  if( NULL == input ) {
    exit(EXIT_FAILURE);
  }

  char * line = NULL;
  size_t len = 0;

  int i = 0;
  int SIZE = 200;
  long num_array[SIZE];
  while(getline(&line, &len, input) != -1) {
    const long num = strtol(line, NULL, 10);
    num_array[i++] = num;
    for(int j=0; j<i-2; j++) {
      for(int k=j+1; k<i-1; k++) {
        if(num_array[j]+num_array[k]+num == 2020) {
          printf("part2: %d\n", num_array[j]*num_array[k]*num);
          break;
        }
      }
    }
  }

  fclose(input);
}
