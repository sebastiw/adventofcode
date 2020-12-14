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

  int south_pt1 = 0;
  int east_pt1 = 0;
  int direction_pt1 = 0;

  int wp_south_pt2 = -1;
  int wp_east_pt2 = 10;
  int boat_south_pt2 = 0;
  int boat_east_pt2 = 0;

  int tmp;
  while(-1 != getline(&line, &len, input)) {
    int num = strtol((line+1), NULL, 10);
    switch(line[0]) {
    case 'E':
      east_pt1 += num;
      wp_east_pt2 += num;
      break;
    case 'S':
      south_pt1 += num;
      wp_south_pt2 += num;
      break;
    case 'W':
      east_pt1 -= num;
      wp_east_pt2 -= num;
      break;
    case 'N':
      south_pt1 -= num;
      wp_south_pt2 -= num;
      break;
    case 'L':
      direction_pt1 = (4+direction_pt1-(num/90))%4;
      switch((4-num/90)%4) {
      case 0:
        break;
      case 1:
        tmp = wp_south_pt2;
        wp_south_pt2 = wp_east_pt2;
        wp_east_pt2 = -tmp;
        break;
      case 2:
        wp_south_pt2 = -wp_south_pt2;
        wp_east_pt2 = -wp_east_pt2;
        break;
      case 3:
        tmp = wp_south_pt2;
        wp_south_pt2 = -wp_east_pt2;
        wp_east_pt2 = tmp;
        break;
      }
      break;
    case 'R':
      direction_pt1 = (4+direction_pt1+(+num/90))%4;
      switch((4+num/90)%4) {
      case 0:
        break;
      case 1:
        tmp = wp_south_pt2;
        wp_south_pt2 = wp_east_pt2;
        wp_east_pt2 = -tmp;
        break;
      case 2:
        wp_south_pt2 = -wp_south_pt2;
        wp_east_pt2 = -wp_east_pt2;
        break;
      case 3:
        tmp = wp_south_pt2;
        wp_south_pt2 = -wp_east_pt2;
        wp_east_pt2 = tmp;
        break;
      }
      break;
    case 'F':
      boat_south_pt2 += num*wp_south_pt2;
      boat_east_pt2 += num*wp_east_pt2;
      switch(direction_pt1) {
      case 0:
        // east
        east_pt1 += num;
        break;
      case 1:
        // south
        south_pt1 += num;
        break;
      case 2:
        // west
        east_pt1 -= num;
        break;
      case 3:
        // north
        south_pt1 -= num;
        break;
      }
      break;
    }
  }

  printf("Part 1: %d\n", abs(south_pt1)+abs(east_pt1));
  printf("Part 2: %d\n", abs(boat_south_pt2)+abs(boat_east_pt2));

  fclose(input);

  exit(EXIT_SUCCESS);
}
