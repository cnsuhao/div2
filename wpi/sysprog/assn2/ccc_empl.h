#ifndef CCC_EMPL_H
#define CCC_EMPL_H

#include <string>

using namespace std;

/**
   A basic employee class that is used in many examples
   in the book "Computing Concepts with C++ Essentials"
*/
class Employee
{
public:
   /**
      Constructs an employee with empty name and no salary.
   */
   Employee();
   /**
      Constructs an employee with a given name and salary.
      @param employee_name the employee name
      @param initial_salary the initial salary
   */
   Employee(string employee_name, double initial_salary);
   /**
      Sets the salary of this employee.
      @param new_salary the new salary value
   */
   void set_salary(double new_salary);
   /**
      Gets the salary of this employee.
      @return the current salary
   */
   double get_salary() const;
   /**
      Gets the name of this employee.
      @return the employee name
   */
   string get_name() const;

   /**
      Prints out the name and salary of the employee
   */
   void print() const;

   /**
   dynamically bound print method
  */
  virtual void vprint() const;

  /**
   a method to tell if current object makes more than parameter
   @param e given employee
   @return true if this employee makes more
  */
  bool makes_more1(Employee e) const;

/**
   a method to tell if current object makes more than parameter
   @param e pointer to a given employee
   @return true if this employee makes more
  */
  bool makes_more2(Employee* e) const;

protected:
   string name;
   double salary;
};

#endif
