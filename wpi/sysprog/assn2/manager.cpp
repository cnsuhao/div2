#include "ccc_empl.h"
#include "manager.h"
#include "highlevelmanager.h"
#include <iostream>

/**
   Create a manager with a name, salary, department, and number of subordinates
   @param man_name the name of the manager
   @param initial_salary the salary of the manager
   @param man_dept the department of the manager
   @param subords the number of subordinates
*/
Manager::Manager(string man_name, double initial_salary, string man_dept, int subords)
  : Employee(man_name, initial_salary)
{
  name = man_name;
  salary = initial_salary;
  department = man_dept;
  num_of_subords = subords;
}

/**
  a method to print the data stored in the Manager
*/
void Manager::print() const
{
  cout << "Manager-> name: " << name << " Salary: " << salary << " Dept: " << department 
       << " Num of Subordinates: " << num_of_subords << "\n";

}

/**
   a method to dynamically print data stored in the Manager
*/
void Manager::vprint() const
{
  cout << "Manager-> name: " << name << " Salary: " << salary << " Dept: " << department 
       << " Num of Subordinates: " << num_of_subords << "\n";

}
