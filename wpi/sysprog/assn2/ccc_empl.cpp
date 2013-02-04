#include "ccc_empl.h"
#include <iostream>

/**
 * Used to create a default employeed with a salary of 0
 */
Employee::Employee()
{  
   salary = 0;
}

/**
 *  Create a employee with a name and a salary
 *  @param employee_name the name of the employee
 *  @param initial_salary the salaray the employee
 */
Employee::Employee(string employee_name, double initial_salary)
{  
   name = employee_name;
   salary = initial_salary;
}

/**
 * Modify the salary for employee
 * @param new_salary what to set the employee's salary to
 */
void Employee::set_salary(double new_salary)
{  
   salary = new_salary;
}

/**
 * Accessor method to get the salary back
 * @return the salary of the employee
 */
double Employee::get_salary() const
{  
   return salary;
}

/**
 * Accessor method to get the employee's name
 * @return the name of the employee
 */
string Employee::get_name() const
{  
   return name;
}

/**
 * Print the employee's name and salary
 */
void Employee::print() const 
{
  cout << "Employee-> name: " << name << " Salary: " << salary << "\n";
}

/**
 * Dynamically print the employee's name and salary
 */
void Employee::vprint() const 
{
  cout << "Employee-> name: " << name << " Salary: " << salary << "\n";
}

  /**
   a method to tell if current object makes more than parameter
   @param e given employee
   @return true if this employee makes more
  */
bool Employee::makes_more1(Employee e) const 
{
  return salary>e.get_salary();
}

/**
   a method to tell if current object makes more than parameter
   @param e pointer to a given employee
   @return true if this employee makes more
  */
bool Employee::makes_more2(Employee* e) const
{
  return salary>(*e).get_salary();
}
