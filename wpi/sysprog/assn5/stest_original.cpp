#include <cstdio>
#include <cstdlib>
#include <string.h>
#include <iostream>
using namespace std;

// Define some structures
struct Employee {
  int salary; // Annual salary in UK Pounds Sterling.
  char *name; // Pointer to character string holding name of employee.
              // MUST be dynamically allocated from the heap.
};

// function prototypes
void printEmployee(struct Employee *employee);
void outputEmployee(FILE *stream, struct Employee *employee);

int main() {
  printf("Starting program stest.\n"); 

  // Anybody recognize these names?
  struct Employee harry; // Define a local variable (a struct).
  harry.salary = 50000;
  harry.name = strdup("Harry Palmer"); // Make a dynamic copy.

  struct Employee bluejay; // Define a local variable (a struct).
  bluejay.salary = 100000;
  bluejay.name = strdup("Erik Grantby"); // Make a dynamic copy.

  // Output the employees to stdout.
  printEmployee(&harry);
  printEmployee(&bluejay);

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
  
#ifdef DEBUG
  fprintf(outfile, "This program was compiled with debug\n");
#endif

  fclose(outfile);

  printf("Ending program stest.\n"); 
  return 0;
}

/** Prints employee's info to console
    @param employee a pointer to an employee struct
*/
void printEmployee(struct Employee *employee) {
  fprintf(stdout, "Employee. Name = %s, Salary = %d\n",
	  employee->name, employee->salary);
}

/** Prints employee's info to file
    @param stream pointer to file stream
    @param employee pointer to Employee struct
*/
void outputEmployee(FILE *stream, struct Employee *employee) {
  fprintf(stream, "Employee. Name = %s, Salary = %d\n",
	  employee->name, employee->salary);
}
