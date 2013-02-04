#ifndef MANAGER_H
#define MANAGER_H

using namespace std;

#include "ccc_empl.h"
#include <iostream>

class Manager : public Employee {
 public:

  /**
   Create a manager with a name, salary, department, and number of subordinates
   @param man_name the name of the manager
   @param initial_salary the salary of the manager
   @param man_dept the department of the manager
   @param subords the number of subordinates
  */
  Manager(string man_name, double initial_salary, string man_dept, int subords);

  /**
   a method to print the information stored in the manager object
  */
  void print() const;

  /**
   dynamically bound print method
  */
  virtual void vprint() const;

 protected:
  string department;
  int num_of_subords;
};

#endif
