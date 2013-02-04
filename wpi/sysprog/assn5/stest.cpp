#include <iostream>
#include <cstdlib>
#include <cstdio>
#include <string.h>
#include "employee.h"
using namespace std;

int main() {
  printf("Starting program stest.\n"); 

  // Anybody recognize these names?
  struct Employee harry; // Define a local variable (a struct).
  harry.salary = 50000;
  harry.department = strdup("barber");
  harry.name = strdup("Harry Palmer"); // Make a dynamic copy.

  struct Employee bluejay; // Define a local variable (a struct).
  bluejay.salary = 100000;
  bluejay.department = strdup("bird");
  bluejay.name = strdup("Erik Grantby"); // Make a dynamic copy.

  // Output the employees to stdout.
  printEmployee(&harry);
  printEmployee(&bluejay);

  // Bew Employees
  Employee* barry = makeEmployee(20000, "Barry", "IT");
  Employee* robin = makeEmployee(100, "Robin", "Sales");

  // print to stdout
  printEmployee(barry);
  printEmployee(robin);

#ifdef DEBUG
  printf("This program was compiled with debug\n");
#endif

  // Output the employees to a file.
  FILE *outfile = fopen("stest.txt", "w"); // Open or create file for writing
  
  if(outfile == NULL) {
    fprintf(stderr, "Could not write to file\n");
    return 1;
  }

  outputEmployee(outfile, &harry);
  outputEmployee(outfile, &bluejay);
  outputEmployee(outfile, barry);
  outputEmployee(outfile, robin);
  
#ifdef DEBUG
  fprintf(outfile, "This program was compiled with debug\n");
#endif

  fclose(outfile);

  printf("Ending program stest.\n"); 
  return 0;
}
