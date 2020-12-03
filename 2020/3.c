#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int check_slope(char*, int, int*, int);

int main()
{
  FILE *input = fopen("3.input", "r");
  if( NULL == input ) {
    exit(EXIT_FAILURE);
  }

  char * line = NULL;
  size_t len = 0;

  int pos11 = 0;
  int pos31 = 0;
  int pos51 = 0;
  int pos71 = 0;
  int pos12 = 0;
  int line_len = 0;
  int num_trees_11 = 0;
  int num_trees_31 = 0;
  int num_trees_51 = 0;
  int num_trees_71 = 0;
  int num_trees_12 = 0;
  int line_count=0;
  while(getline(&line, &len, input) != -1) {
    if(0 == line_count) {
      while(line[line_len] != '\n') {
        line_len++;
      }
    }

    num_trees_11 += check_slope(line, line_len, &pos11, 1);
    num_trees_31 += check_slope(line, line_len, &pos31, 3);
    num_trees_51 += check_slope(line, line_len, &pos51, 5);
    num_trees_71 += check_slope(line, line_len, &pos71, 7);
    if(0 == line_count % 2) {
      num_trees_12 += check_slope(line, line_len, &pos12, 1);
    }
    line_count++;
  }

  printf("trees: %d, %d, %d, %d, %d\n", num_trees_11, num_trees_31, num_trees_51, num_trees_71, num_trees_12);
  printf("product: %d\n", num_trees_11*num_trees_31*num_trees_51*num_trees_71*num_trees_12);

  fclose(input);

  exit(EXIT_SUCCESS);
}

int check_slope(char* line, int line_len, int* pos, int k) {
  int r = ('#' == line[*pos]);
  *pos=(*pos+k)%line_len;
  return r;
}
