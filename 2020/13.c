#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#define MAX_SLOTS 9

int main()
{
  FILE *input = fopen("13.input", "r");
  if( NULL == input ) {
    printf("File not found\n");
    exit(EXIT_FAILURE);
  }

  char * line = NULL;
  size_t len = 0;

  if(-1 == getline(&line, &len, input)) {
    exit(EXIT_FAILURE);
  }
  int earliest_depart = strtol(line, NULL, 10);

  if(-1 == getline(&line, &len, input)) {
    exit(EXIT_FAILURE);
  }
  char delim[] = ",";
  char *ptr = strtok(line, delim);
  int smallest_depart = INT_MAX;
  int smallest_bus = INT_MAX;
  int offset = 0;
  int first = 0;
  int slots[MAX_SLOTS];
  int offsets[MAX_SLOTS];
  int biggest_idx = 0;
  for(int i=0; i < MAX_SLOTS; i++) {
    slots[i] = 0;
    offsets[i] = 0;
  }
  int i = 0;
  while(ptr != NULL) {
    if(strcmp("x", ptr) == 0) {
    } else {
      int bus = strtol(ptr, NULL, 10);
      slots[i] = bus;
      offsets[i] = offset;
      int departs = bus - (earliest_depart % bus);
      if(departs < smallest_depart) {
        smallest_bus = bus;
        smallest_depart = departs;
      }
      if(slots[biggest_idx] < slots[i]){
        biggest_idx = i;
      }
      i++;
    }
    offset++;
    ptr = strtok(NULL, delim);
  }

  for(int i = 0; i < MAX_SLOTS; i++) {
    printf("%d: a + %d %% %d == 0\n", i, offsets[i], slots[i]);
  }

  /* Bruteforce not working well here */
  unsigned long a = 100000000000000-(100000000000000%slots[biggest_idx] + offsets[biggest_idx]);
  /*
  printf("Starting at %lu, biggest: %d, offset: %d\n", a, slots[biggest_idx], offsets[biggest_idx]);
  int max = 1;
  for(; max; a+=slots[biggest_idx]) {
    max=0;
    for(int i = 0; i < MAX_SLOTS; i++) {
      max+=(a+offsets[i])%slots[i];
    }
    if(max > 590) {
      printf("Found %d at %lu\n", max, a);
    }
  }
  for(int i = 0; i < MAX_SLOTS; i++) {
    printf("%lu %% %d == %d (%d) [%d]\n", a, slots[i], offsets[i], (a%slots[i] != offsets[i]), i);
  }
  */

  printf("Part 1: %d\n", smallest_bus*smallest_depart);
  printf("Part 2: %lu\n", a);

  fclose(input);

  exit(EXIT_SUCCESS);
}
