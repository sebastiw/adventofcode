#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int count_unique(int [], int);
int reset(int [], int*);

int main()
{
  FILE *input = fopen("6.input", "r");
  if( NULL == input ) {
    printf("File not found\n");
    exit(EXIT_FAILURE);
  }

  char * line = "\n";
  size_t len = 0;
  int ans[27] = {0};
  int us_1 = 0;
  int us_2 = 0;
  int group_size = 0;
  while(1) {
    if(-1 == getline(&line, &len, input)) {
      us_1 += count_unique(ans, 1);
      us_2 += count_unique(ans, group_size);
      break;
    }
    if(strcmp("\n", line) == 0) {
      us_1 += count_unique(ans, 1);
      us_2 += count_unique(ans, group_size);
      reset(ans, &group_size);
      continue;
    }
    for(int i = 0; line[i] != '\n' && line[i] != '\0'; i++) {
      ans[line[i] - 'a']++;
    }
    group_size++;
  };

  printf("uniques - part 1: %d\n", us_1);
  printf("uniques - part 2: %d\n", us_2);

  fclose(input);

  exit(EXIT_SUCCESS);
}

int count_unique(int ans[], int group_size) {
  int count = 0;
  for(int i = 0; i < 27; i++) {
    if(ans[i] >= group_size) {
      count++;
    }
  }
  return count;
}

int reset(int ans[], int* group_size) {
  for(int i = 0; i < 27; i++) {
    ans[i] = 0;
  }
  *group_size = 0;
  return 0;
}
