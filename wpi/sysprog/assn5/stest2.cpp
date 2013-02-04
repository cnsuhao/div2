#include <iostream>
#include <stdlib.h>
#include "employee.h"
#include <cstdlib>
#include <cstdio>
#include <string.h>
using namespace std;

int main() {
  int i;

  printf("Starting program stest2.\n"); 

  printf("How many Employees?  ");
  int num;
  scanf("%d", &num);

  Employee** employees = (Employee**) calloc(num, sizeof(Employee*));
  
  Employee** emppoint = employees;
  for(i=0; i<num; i++) {
    *emppoint = promptEmployee();
    emppoint++;
  }

  FILE* outfile = fopen("stest2.txt","w");

  if(outfile == NULL) {
    fprintf(stderr, "Could not write to file\n");
    return 1;
  }

  emppoint = employees;
  for(i=0; i<num; i++) {
    printEmployee(*emppoint);
    outputEmployee(outfile, *emppoint);
    emppoint++;
  }
  
  clear(employees, num);

  return 0;
}
