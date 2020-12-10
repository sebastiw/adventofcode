#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define NOP 0
#define ACC 1
#define JMP 2

int main()
{
  FILE *input = fopen("8.input", "r");
  if( NULL == input ) {
    printf("File not found\n");
    exit(EXIT_FAILURE);
  }

  char * line = "\n";
  int MAX_LINES=597;
  int instructions[MAX_LINES];
  int values[MAX_LINES];
  int run[MAX_LINES];
  for(int i=0; i<MAX_LINES; i++) {
    run[i] = 0;
  }
  int acc = 0;
  size_t len = 0;
  int i = 0;
  while(-1 != getline(&line, &len, input)) {
    values[i] = strtol(&line[3], NULL, 10);
    if(strstr(line, "nop")) {
      instructions[i] = NOP;
    } else if(strstr(line, "acc")) {
      instructions[i] = ACC;
    } else if(strstr(line, "jmp")) {
      instructions[i] = JMP;
    }
    i++;
  }
  MAX_LINES=i;
  for(i=0; run[i] == 0; ) {
    run[i]++;
    if(ACC == instructions[i]) {
      acc += values[i];
    } else if (JMP == instructions[i]) {
      i += values[i];
      continue;
    }
    i++;
  }
  printf("part1 - %d\n", acc);
  for(i=0; i<MAX_LINES; i++) {
    run[i] = 0;
  }
  for(int j=0; j<MAX_LINES; j++) {
    if(NOP == instructions[j]) {
      instructions[j] = JMP;
    } else if(JMP == instructions[j]) {
      instructions[j] = NOP;
    } else {
      continue;
    }
    for(i=0; run[i] == 0; ) {
      run[i]++;
      if(ACC == instructions[i]) {
        acc += values[i];
      } else if (JMP == instructions[i]) {
        i += values[i];
        continue;
      }
      i++;
    }
    if(run[MAX_LINES] == 1) {
      printf("part 2: %d\n", acc);
      break;
    } else {
      for(i=0; i<MAX_LINES; i++) {
        run[i] = 0;
        acc = 0;
      }
      if(NOP == instructions[j]) {
        instructions[j] = JMP;
      } else if(JMP == instructions[j]) {
        instructions[j] = NOP;
      }
    }
  }

  fclose(input);

  exit(EXIT_SUCCESS);
}
