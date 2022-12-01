#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#define MAX_SLOTS 580
#define BIN_LEN (32)

unsigned long mask_value(unsigned long, char*);
static char*  integer_to_bitstring(int, int, char *);

int main()
{
  FILE *input = fopen("14.test.input", "r");
  if( NULL == input ) {
    printf("File not found\n");
    exit(EXIT_FAILURE);
  }

  char * line = NULL;
  size_t len = 0;

  int addresses[MAX_SLOTS];
  unsigned long mem[MAX_SLOTS];
  for(int i=0; i<MAX_SLOTS; i++) {
    addresses[i] = -1;
    mem[i] = 0;
  }

  int start = 0;
  char mask[37] = {0};
  int addr = 0;
  unsigned long value = 0;
  while(-1 != getline(&line, &len, input)) {
    if(NULL != strstr(line, "mask = ")) {
      strncpy(mask, line+7, 36);
      mask[37] = '\0';
      printf("mask: %s\n", mask);
    } else if(NULL != strstr(line, "mem[")) {
      addr = strtol(&line[strlen("mem[")], NULL, 10);
      value = strtol(strstr(line, "] = ")+strlen("] = "), NULL, 10);
      for(int i = 0; i<MAX_SLOTS; i++) {
        if((addresses[i] == addr) || (addresses[i] == -1)) {
          addresses[i] = addr;
          printf("length of mask = %d, %s\n", strlen(mask), mask);
          mem[i] = mask_value(value, mask);
          break;
        }
      }
    }
  }

  int sum=0;
  for(int i=0; i < MAX_SLOTS; i++) {
    if(addresses[i] == -1) {
      break;
    } else {
      sum+=mem[i];
    }
  }

  printf("Part 1: %d\n", sum);
  printf("Part 2: %lu\n", 0);

  fclose(input);

  exit(EXIT_SUCCESS);
}

unsigned long mask_value(unsigned long value, char* mask) {
  char str[strlen(mask)];
  //integer_to_bitstring(value, strlen(mask), str);
  unsigned long mask_1 = 0, mask_2 = 0;
  for(int i=0; i < strlen(mask); i++) {
    mask_1 <<= 1;
    mask_2 <<= 1;
    printf("%d Shifted mask_2: %lu\n", i, mask_1);
    switch(mask[i]) {
    case 'X':
    case '1':
      printf("// %c\n", mask[i]);
      mask_1++;
      break;
    case '0':
      mask_2++;
      break;
    }
  }

  printf("mask_o: %s\n", mask);
  printf("mask_1: %lu\n", mask_1);
  printf("mask_2: %lu\n", mask_2);

  return (value ^ mask_2) | mask_1;
}

static char *  integer_to_bitstring(int value, int length, char * str) {
  int i, n = 2, tmp;
  char buf[BIN_LEN+1];
  /*
  for(i = 0, tmp = value; i<BIN_LEN; ++i) {
    if(tmp/2 > 0) {
      n++;
    }
    tmp /= 2;
  }
  */
  n=length;
  printf("v: %d, %d\n", value, length);
  for(i = 1, tmp = value; i<n; ++i) {
    if(tmp % 2 != 0) {
      buf[n-i-1] = '1';
    } else {
      buf[n-i-1] = '0';
    }
    tmp /= 2;
  }
  buf[n-1] = '\0';
  strcpy(str, buf);
  return str;
}
