#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>

#define MAX_LINES 101
#define MAX_COLUMNS 93

int print_matrix(int [MAX_LINES][MAX_COLUMNS], int, int);
int occupied_adjacent_seats_1(int [MAX_LINES][MAX_COLUMNS], int, int);
int occupied_adjacent_seats_2(int [MAX_LINES][MAX_COLUMNS], int, int);

int main()
{
  FILE *input = fopen("11.input", "r");
  if( NULL == input ) {
    printf("File not found\n");
    exit(EXIT_FAILURE);
  }

  char * line = "\n";
  size_t len = 0;

  int seat_1[MAX_LINES][MAX_COLUMNS];
  int seat_2[MAX_LINES][MAX_COLUMNS];
  int seat_cpy[MAX_LINES][MAX_COLUMNS];
  for(int i=0; i<MAX_LINES; i++) {
    for(int j=0; j<MAX_COLUMNS; j++) {
      seat_1[i][j] = 0;
      seat_2[i][j] = 0;
    }
  }
  int row=1;
  int col=1;
  while(-1 != getline(&line, &len, input)) {
    for(col=1; '\n' != line[col-1]; col++) {
      switch(line[col-1]) {
      case 'L':
        seat_1[row][col] = -1;
        seat_2[row][col] = -1;
        break;
      case '#':
        seat_1[row][col] = 1;
        seat_2[row][col] = 1;
        break;
      case '.':
        seat_1[row][col] = 0;
        seat_2[row][col] = 0;
        break;
      }
    }
    row++;
  }

  while(memcmp(seat_cpy, seat_1, MAX_LINES*MAX_COLUMNS * sizeof(int))) {
    memcpy(seat_cpy, seat_1, MAX_LINES*MAX_COLUMNS * sizeof(int));
    for(int i=1; i<row; i++) {
      for(int j=1; j<col; j++) {
        int occupied = occupied_adjacent_seats_1(seat_cpy, i, j);
        if(-1 == seat_1[i][j] && occupied == 0) {
          seat_1[i][j] = 1;
        } else if(1 == seat_1[i][j] && occupied >= 4){
          seat_1[i][j] = -1;
        }
      }
    }
  }

  while(memcmp(seat_cpy, seat_2, MAX_LINES*MAX_COLUMNS * sizeof(int))) {
    memcpy(seat_cpy, seat_2, MAX_LINES*MAX_COLUMNS * sizeof(int));
    for(int i=1; i<row; i++) {
      for(int j=1; j<col; j++) {
        int occupied = occupied_adjacent_seats_2(seat_cpy, i, j);
        if(-1 == seat_2[i][j] && occupied == 0) {
          seat_2[i][j] = 1;
        } else if(1 == seat_2[i][j] && occupied >= 5){
          seat_2[i][j] = -1;
        }
      }
    }
  }

  int occupied_seats_1=0;
  int occupied_seats_2=0;
  for(int i=1; i<row; i++) {
    for(int j=1; j<col; j++) {
      if(seat_1[i][j] == 1) {
        occupied_seats_1++;
      }
      if(seat_2[i][j] == 1) {
        occupied_seats_2++;
      }
    }
  }
  printf("Part 1: %d\n", occupied_seats_1);
  printf("Part 2: %d\n", occupied_seats_2);

  fclose(input);

  exit(EXIT_SUCCESS);
}

int print_matrix(int seat[MAX_LINES][MAX_COLUMNS], int row, int col) {
  for(int i=1; i<row; i++) {
    for(int j=1; j<col; j++) {
      if(seat[i][j] < 0) {
        printf("L");
      } else if(seat[i][j] > 0) {
        printf("#");
      } else {
        printf(".");
      }
    }
    printf("\n");
  }
}

int occupied_adjacent_seats_1(int seat[MAX_LINES][MAX_COLUMNS], int i, int j) {
  return
    (seat[i-1][j-1] > 0) + (seat[i-1][j] > 0) + (seat[i-1][j+1] > 0) +
    (seat[i][j-1] > 0)   + 0                  + (seat[i][j+1] > 0) +
    (seat[i+1][j-1] > 0) + (seat[i+1][j] > 0) + (seat[i+1][j+1] > 0);
}

int occupied_adjacent_seats_2(int seat[MAX_LINES][MAX_COLUMNS], int i, int j) {
  int num = 0;
  int a=0;
  for(a=i+1; (seat[a][j] == 0) && a < MAX_LINES-1; a++) {}
  if(seat[a][j] > 0) {
    num++;
  }
  for(a=i-1; (seat[a][j] == 0) && a > 0; a--) {}
  if(seat[a][j] > 0) {
    num++;
  }
  for(a=j+1; (seat[i][a] == 0) && a < MAX_COLUMNS-1; a++) {}
  if(seat[i][a] > 0) {
    num++;
  }
  for(a=j-1; (seat[i][a] == 0) && a > 0; a--) {}
  if(seat[i][a] > 0) {
    num++;
  }
  for(a=1; (seat[i+a][j+a] == 0) && i+a < MAX_LINES-1 && j+a < MAX_COLUMNS-1; a++) {}
  if(seat[i+a][j+a] > 0) {
    num++;
  }
  for(a=1; (seat[i+a][j-a] == 0) && i+a < MAX_LINES-1 && j-a > 0; a++) {}
  if(seat[i+a][j-a] > 0) {
    num++;
  }
  for(a=1; (seat[i-a][j-a] == 0) && i-a > 0 && j-a > 0; a++) {}
  if(seat[i-a][j-a] > 0) {
    num++;
  }
  for(a=1; (seat[i-a][j+a] == 0) && i-a > 0 && j+a < MAX_COLUMNS-1; a++) {}
  if(seat[i-a][j+a] > 0) {
    num++;
  }
  return num;
}
