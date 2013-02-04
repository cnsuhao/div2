#include "highlevelmanager.h"
#include <iostream>

/**
   Create a high level manager with a name, salary, department, and number of 
   subordinates
   @param man_name the name of the high level manager
   @param initial_salary the salary of the high level manager
   @param man_dept the department of the high level manager
   @param subords the number of subordinates
  */
HighLevelManager::HighLevelManager(string man_name, double initial_salary, string man_dept, int subords)
  : Manager(man_name, initial_salary, man_dept, subords)
{}

/**
 a method to print the data in a high level manager
*/
void HighLevelManager::print() const
{
  cout << "High Level ";
  Manager::print();
}

/**
 a method to dynamically print the data in a high level manager
*/
void HighLevelManager::vprint() const
{
  cout << "High Level ";
  Manager::print();
}
