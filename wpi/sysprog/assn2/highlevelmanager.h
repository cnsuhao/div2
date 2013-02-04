#ifndef HIGHLEVELMANAGER_H
#define HIGHLEVELMANAGER_H

using namespace std;

#include "manager.h"
#include <iostream>

class HighLevelManager : public Manager {
 public:

  /**
   Create a high level manager with a name, salary, department, and number of 
   subordinates
   @param man_name the name of the high level manager
   @param initial_salary the salary of the high level manager
   @param man_dept the department of the high level manager
   @param subords the number of subordinates
  */
  HighLevelManager(string man_name, double initial_salary, string man_dept, int subords);

  /**
   a method to print the information stored in the manager object
  */
  void print() const;

  /**
   dynamically bound print method
  */
  virtual void vprint() const;

};

#endif
