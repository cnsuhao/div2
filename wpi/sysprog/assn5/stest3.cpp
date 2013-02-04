#include <iostream>
#include <stdlib.h>
#include <cstdlib>
#include <cstdio>
#include <string.h>
#include "employee.h"
using namespace std;

int main() {
  int i;

  printf("Starting program stest3.\n"); 

  FILE* in = fopen("stest2.txt","rt");

  Employee* emp;

  while(true) {
    emp = readEmployee(in);

    if(emp==NULL) break;

    printEmployee(emp);
  }

  return 0;
}
