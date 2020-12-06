#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int valid_pass_1(char[], char[], char[], char[], char[], char[], char[], char[]);
int valid_pass_2(char[], char[], char[], char[], char[], char[], char[], char[]);

int main()
{
  FILE *input = fopen("4.input", "r");
  if( NULL == input ) {
    exit(EXIT_FAILURE);
  }

  char * line = NULL;
  size_t len = 0;

  char delim[] = " :\n";
  char byr[40];
  char iyr[40];
  char eyr[40];
  char hgt[40];
  char hcl[40];
  char ecl[40];
  char pid[40];
  char cid[40];
  int valid_passes_1 = 0;
  int valid_passes_2 = 0;
  while(1) {
    if(-1 == getline(&line, &len, input)) {
      valid_passes_1 += valid_pass_1(byr, iyr, eyr, hgt, hcl, ecl, pid, cid);
      valid_passes_2 += valid_pass_2(byr, iyr, eyr, hgt, hcl, ecl, pid, cid);
      break;
    } else if(strcmp("\n", line) == 0) {
      valid_passes_1 += valid_pass_1(byr, iyr, eyr, hgt, hcl, ecl, pid, cid);
      valid_passes_2 += valid_pass_2(byr, iyr, eyr, hgt, hcl, ecl, pid, cid);
      strcpy(byr, "");
      strcpy(iyr, "");
      strcpy(eyr, "");
      strcpy(hgt, "");
      strcpy(hcl, "");
      strcpy(ecl, "");
      strcpy(pid, "");
      strcpy(cid, "");
    } else {
      char *ptr = strtok(line, delim);
      while(ptr != NULL) {
        if(strcmp("byr", ptr) == 0) {
          ptr = strtok(NULL, delim);
          strcpy(byr, ptr);
        } else
        if(strcmp("iyr", ptr) == 0) {
          ptr = strtok(NULL, delim);
          strcpy(iyr, ptr);
        } else
        if(strcmp("eyr", ptr) == 0) {
          ptr = strtok(NULL, delim);
          strcpy(eyr, ptr);
        } else
        if(strcmp("hgt", ptr) == 0) {
          ptr = strtok(NULL, delim);
          strcpy(hgt, ptr);
        } else
        if(strcmp("hcl", ptr) == 0) {
          ptr = strtok(NULL, delim);
          strcpy(hcl, ptr);
        } else
        if(strcmp("ecl", ptr) == 0) {
          ptr = strtok(NULL, delim);
          strcpy(ecl, ptr);
        } else
        if(strcmp("pid", ptr) == 0) {
          ptr = strtok(NULL, delim);
          strcpy(pid, ptr);
        } else
        if(strcmp("cid", ptr) == 0) {
          ptr = strtok(NULL, delim);
          strcpy(cid, ptr);
        }
        ptr = strtok(NULL, delim);
      };
    }
  }
  printf("Valid passes - part 1: %d\n", valid_passes_1);
  printf("Valid passes - part 2: %d\n", valid_passes_2);


  fclose(input);

  exit(EXIT_SUCCESS);
}

int valid_pass_1(char byr[], char iyr[], char eyr[], char hgt[], char hcl[], char ecl[], char pid[], char cid[]) {
  if(strcmp("", byr) == 0 || strcmp("", iyr) == 0 || strcmp("", eyr) == 0 || strcmp("", hgt) == 0 || strcmp("", hcl) == 0 || strcmp("", ecl) == 0 || strcmp("", pid) == 0) {
    return 0;
  } else {
    return 1;
  }
}

int valid_pass_2(char byr[], char iyr[], char eyr[], char hgt[], char hcl[], char ecl[], char pid[], char cid[]) {
  if(strcmp("", byr) == 0 || strcmp("", iyr) == 0 || strcmp("", eyr) == 0 || strcmp("", hgt) == 0 || strcmp("", hcl) == 0 || strcmp("", ecl) == 0 || strcmp("", pid) == 0) {
    return 0;
  } else {
    int byri = strtol(byr, NULL, 10);
    if(1920 > byri || byri > 2002) {
      printf("discarding because byr %d\n", byri);
      return 0;
    }
    int iyri = strtol(iyr, NULL, 10);
    if(2010 > iyri || iyri > 2020) {
      printf("discarding because iyr %d\n", iyri);
      return 0;
    }
    int eyri = strtol(eyr, NULL, 10);
    if(2020 > eyri || eyri > 2030) {
      printf("discarding because eyr %d\n", eyri);
      return 0;
    }
    int hgti = strtol(hgt, NULL, 10);
    char hgtu[2];
    hgtu[0] = hgt[strlen(hgt)-2];
    hgtu[1] = hgt[strlen(hgt)-1];
    if(strcmp("cm", hgtu) != 0 && strcmp("in", hgtu) != 0) {
      printf("discarding because hgt %s not cm/in\n", hgt);
      return 0;
    }
    if((150 > hgti || hgti > 193) && strcmp("cm", hgtu) == 0) {
      printf("discarding because cm %d\n", hgti);
      return 0;
    }
    if((59 > hgti || hgti > 76) && strcmp("in", hgtu) == 0) {
      printf("discarding because in %d\n", hgti);
      return 0;
    }

    if('#' != hcl[0] || strlen(hcl) != 7) {
      printf("discarding because hcl (%s) not # or length 7\n", hcl);
      return 0;
    }
    long hcll = strtol(*(&hcl)+1, NULL, 16);
    if(0 > hcll || hcll > 0xffffff) {
      printf("discarding because hcl: %d\n", hcll);
      return 0;
    }

    if(strcmp("amb", ecl) != 0 && strcmp("blu", ecl) != 0 && strcmp("brn", ecl) != 0  && strcmp("gry", ecl) != 0 && strcmp("grn", ecl) != 0 && strcmp("hzl", ecl) != 0 && strcmp("oth", ecl) != 0) {
      printf("discarding because ecl %s\n", ecl);
      return 0;
    }

    if(strlen(pid) != 9) {
      printf("discarding because pid length\n");
      return 0;
    }
    long long pidll = strtoll(pid, NULL, 10);
    if(0 > pidll || pidll > 999999999) {
      printf("discarding because pidll = %d\n", pidll);
      return 0;
    }
    return 1;
  }
}
