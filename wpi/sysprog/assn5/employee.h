#include <iostream>

// Define some structures
struct Employee {
  int salary; // Annual salary in UK Pounds Sterling.
  char *name; // Pointer to character string holding name of employee.
              // MUST be dynamically allocated from the heap.
  char* department; // Pointer to char string holding department
};

// function prototypes
void printEmployee(struct Employee *employee);
void outputEmployee(FILE *stream, struct Employee *employee);
Employee* makeEmployee(int the_salary, char* the_name, char* department);
Employee* promptEmployee();
void clear(Employee** emps, int size);
Employee* readEmployee(FILE* infile);
