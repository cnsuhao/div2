#include <iostream>
#include <stdlib.h>
#include <stdio.h>
#include <cstdlib>
#include <cstdio>
#include <string.h>
#include "employee.h"

using namespace std;

/** Prints employee's info to console
    @param employee a pointer to an employee struct
*/
void printEmployee(struct Employee *employee) {
  fprintf(stdout, "Employee. Name = %s, Department = %s, Salary = %d\n",
	  employee->name, employee->department, employee->salary);
}

/** Prints employee's info to file
    @param stream pointer to file stream
    @param employee pointer to Employee struct
*/
void outputEmployee(FILE *stream, struct Employee *employee) {
    fprintf(stream, "Employee. Name = %s, Department = %s, Salary = %d\n",
	  employee->name, employee->department, employee->salary);
}

/** Accepts info needed for a new Employee
    @param the_salary salary for Employee
    @param the_name name for Employee
    @param department name of deparment
    @return pointer to new Employee
*/
Employee* makeEmployee(int the_salary, char* the_name, char* the_department) {
  Employee* newEmployee = (Employee*) malloc(sizeof(Employee));
  newEmployee->salary = the_salary;
  newEmployee->name = strdup(the_name);
  newEmployee->department = strdup(the_department);

  return newEmployee;
}

/** Prompts user for employee info and returns a new employee
    @return pointer to new Employee
*/
Employee* promptEmployee() {
  char name[50];
  char dept[50];
  int salary;
  char salstr[20];
  char remainder[20];
  

  printf("Please enter the name for the employee: ");
  scanf("%s",name);
  printf("Please enter the department: ");
  scanf("%s",dept);

  while(true) {
    printf("Please enter salary: ");
    scanf("%s",salstr);
    sscanf(salstr, "%d %s", &salary, remainder);

    if(salary==0 || remainder == "") {
      printf("Try again\n");
      continue;
    }

    break;
  }

  return makeEmployee(salary, name, dept);
}

/** Delete all employees in array
    @param emps array of employee wrappers
    @param size of emp array
*/
void clear(Employee** emps, int size){
  for (int i = 0; i < size; i++)
    {
      delete (emps[i]);
    }
  free(emps);
}

/** Read employee info from file
    @param infile file with employees
    @return pointer to employee
*/
Employee* readEmployee(FILE* infile) {

  char* input;
  int salary;

  int c;

  // get first word of name
  fscanf(infile,"%*s");  
  // check if at end of file
  if(feof(infile)) return NULL;
  // keep getting name
  fscanf(infile,"%*s");
  fscanf(infile,"%*s");

  // get name
  char* name = strdup("");

  bool more = true;
  while(more) {
    fscanf(infile,"%s",input);
    
    c=0;
    char* temp = input;
    while(true) {
      temp++;      
      c++;
      // end of a name word
      if(*temp == '\0') {
	strncat(name, input, c);
	strcat(name, " ");
	break;
      }     
      // end of name
      if(*temp == ',') {
	more = false;
	strncat(name, input, c);
	break;
      }
    }
  }

 // get first word of dept
  fscanf(infile,"%*s");
  fscanf(infile,"%*s");

  // get dept
  char* dept = strdup("");

  more = true;
  while(more) {
    fscanf(infile,"%s",input);
    
    c=0;
    char* temp = input;
    while(true) {
      temp++;      
      c++;
      // end of a name word
      if(*temp == '\0') {
	strncat(dept, input, c);
	strcat(dept, " ");
	break;
      }     
      // end of name
      if(*temp == ',') {
	more = false;
	strncat(dept, input, c);
	break;
      }
    }
  }

  // get salary
  fscanf(infile,"%*s");
  fscanf(infile,"%*s");
  fscanf(infile,"%d",&salary);

  return makeEmployee(salary, name, dept);
}
