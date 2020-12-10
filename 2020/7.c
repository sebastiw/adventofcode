#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int remove_duplicates(char [][30]);
int save_colors(char [][30], char*);

int main()
{
  FILE *input = fopen("7.input", "r");
  if( NULL == input ) {
    printf("File not found\n");
    exit(EXIT_FAILURE);
  }

  char * line = "";
  size_t len = 0;
  char * needle = "shiny gold bag";
  char side_l[100] = {0};
  char side_r[100] = {0};
  char* search;
  char* search_str = " contain ";
  int search_start;
  int num_lines_total = 600;

  char discarded[num_lines_total][30];
  char saved[num_lines_total][30];
  char too_little_info[num_lines_total][30];
  char too_little_info_right[num_lines_total][100];
  while(-1 != getline(&line, &len, input)) {
    search = strstr(line, search_str);
    search_start = search-line;
    if(NULL == search) {
      printf("line (%s) does not contain contain\n", line);
      break;
    }
    strncpy(side_l, line, search_start);
    side_l[search_start] = '\0';
    strncpy(side_r, line+search_start+strlen(search_str), strlen(line)-search_start-strlen(search_str));
    side_r[strstr(side_r, "\n")-side_r] = '\0';
    if(strcmp(side_r, "no other bags.") == 0) {
      save_colors(discarded, side_l);
    } else if(strstr(side_r, needle)) {
      save_colors(saved, side_l);
    } else if(strstr(side_l, needle)) {
      save_colors(discarded, side_r);
    } else {
      int a = save_colors(too_little_info, side_l);
      strcpy(too_little_info_right[a], side_r);
    }
  }
  int all_clear;
  do {
    all_clear=1;
    for(int i = 0; i < num_lines_total; i++) {
      for(int a_ix = 0; '\0' != saved[a_ix][0]; a_ix++) {
        if(strstr(too_little_info_right[i], saved[a_ix])) {
          all_clear=0;
          save_colors(saved, too_little_info[i]);
          too_little_info[i][0] = '\0';
          too_little_info_right[i][0] = '\0';
        }
      }
      for(int a_ix = 0; '\0' != discarded[a_ix][0]; a_ix++) {
        if(0 == strcmp(too_little_info[i], discarded[a_ix])) {
          all_clear=0;
          save_colors(discarded, too_little_info_right[i]);
          too_little_info[i][0] = '\0';
          too_little_info_right[i][0] = '\0';
        }
      }
    }
  } while(!all_clear);
  remove_duplicates(saved);
  remove_duplicates(discarded);

  for(int a_ix = 0; '\0' != saved[a_ix][0]; a_ix++) {
    printf("saved: %d: %s\n", a_ix, saved[a_ix]);
  }
  for(int a_ix = 0; '\0' != discarded[a_ix][0]; a_ix++) {
    printf("discarded: %d: %s\n", a_ix, discarded[a_ix]);
  }

  fclose(input);

  exit(EXIT_SUCCESS);
}

int remove_duplicates(char array[][30]) {
  int size;
  for(size=0; '\0' != array[size][0]; size++) {}
  for(int i=0; i<size; i++) {
    for(int j=i+1; j<size; j++) {
      if(0 == strcmp(array[i], array[j])) {
        for(int k=j; k<size; k++) {
          strcpy(array[k], array[k+1]);
        }
        size--;
        j--;
      }
    }
  }
  return 0;
}

int save_colors(char array[][30], char* string) {
  char* delimiter = ",";
  char* ptr;
  long num_bags;
  char* stop_here;
  int begin = 0;
  char* token = strtok(string, delimiter);
  int a_ix;
  for(a_ix=0; '\0' != array[a_ix][0]; a_ix++) {}
  while(token != NULL) {
    num_bags = strtol(token, &ptr, 10);
    stop_here = strstr(ptr, " bag");
    if(NULL != stop_here) {
      *stop_here = '\0';
    }
    if(' ' == ptr[0]) {
      begin = 1;
    } else {
      begin = 0;
    }

    strcpy(array[a_ix++], ptr+begin);
    token = strtok(NULL, delimiter);
  }
  return a_ix-1;
}
