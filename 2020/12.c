#include <stdio.h>
#include <stdlib.h>

int main()
{
  FILE *input = fopen("12.input", "r");
  if( NULL == input ) {
    printf("File not found\n");
    exit(EXIT_FAILURE);
  }

  char * line = "\n";
  size_t len = 0;

  int south = 0;
  int east = 0;
  int direction = 0;

  while(-1 != getline(&line, &len, input)) {
    int num = strtol((line+1), NULL, 10);
    switch(line[0]) {
    case 'E':
      east += num;
      break;
    case 'S':
      south += num;
      break;
    case 'W':
      east -= num;
      break;
    case 'N':
      south -= num;
      break;
    case 'L':
      direction = (4+direction-(num/90))%4;
      break;
    case 'R':
      direction = (4+direction+(+num/90))%4;
      break;
    case 'F':
      switch(direction) {
      case 0:
        // east
        east += num;
        break;
      case 1:
        // south
        south += num;
        break;
      case 2:
        // west
        east -= num;
        break;
      case 3:
        // north
        south -= num;
        break;
      }
      break;
    }
  }

  printf("Part 1: %d\n", abs(south)+abs(east));

  fclose(input);

  exit(EXIT_SUCCESS);
}
