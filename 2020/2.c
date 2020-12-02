#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int loop_part_1(long, long, char*, char*);
int loop_part_2(long, long, char*, char*);

int main()
{
  FILE *input = fopen("2.input", "r");
  if( NULL == input ) {
    exit(EXIT_FAILURE);
  }

  char * line = NULL;
  size_t len = 0;

  char delim[] = "- :\n";
  long min;
  long max;
  char* c;
  char* z;
  int is_ok_1 = 0;
  int is_ok_2 = 0;
  while(getline(&line, &len, input) != -1) {
    char *ptr = strtok(line, delim);
    min = strtol(ptr, NULL, 10);
    ptr = strtok(NULL, delim);
    max = strtol(ptr, NULL, 10);
    ptr = strtok(NULL, delim);
    c = ptr;
    ptr = strtok(NULL, delim);
    z = ptr;

    if(loop_part_1(min, max, c, z)) {
      is_ok_1++;
    }
    if(1 == loop_part_2(min, max, c, z)) {
      is_ok_2++;
    }
  }
  printf("part_1: %d\n", is_ok_1);
  printf("part_2: %d\n", is_ok_2);

  fclose(input);

  exit(EXIT_SUCCESS);
}


int loop_part_1(long min, long max, char* c, char* z) {
  int matches = 0;
  for(int m=0; z[m]; m++) {
    if(z[m] == *c) {
      matches++;
    }
  }

  return (matches >= min && matches <= max);
}

int loop_part_2(long min, long max, char* c, char* z) {
  return (z[min-1] == *c) + (z[max-1] == *c);
}
