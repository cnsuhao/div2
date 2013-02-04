#include <iostream>

using namespace std;

#include "ccc_empl.h"
#include "manager.h"
#include "highlevelmanager.h"

/**
 * Test employee class
 */
int main()
{  
   Employee harry("Hacker, Harry", 45000.00);
   Manager larry("Lacker, Larry", 100000.00, "BossMan", 28);
   HighLevelManager barry("Backer, Barry", 20.00, "Boss's Boss", 2000);

   double new_salary = harry.get_salary() + 3000;
   harry.set_salary(new_salary);

   cout << "\n\tOBJECTS VS POINTERS\n\n";

   cout << "No Pointers: \n";

   harry.print();
   larry.print();
   barry.print();

   cout << "\nWith Pointers: \n";

   Employee* emp = new Employee("Acker, Arry", 45000.00);
   Manager* mang = new Manager("Tacker, Tarry", 100000.00, "BossMan", 28);
   HighLevelManager* hlm = new HighLevelManager("Nacker, Narry", 20.00, "Boss's Boss", 2000);

   (*emp).print();
   mang->print();
   (*hlm).print();

   cout << "\n\n\tPRINT vs VPRINT (print displayed first):\n\n";

   cout << "HighLevel Manager in a Manager:\n";
   Manager* tmp_mang = hlm;
   (*tmp_mang).print();
   (*tmp_mang).vprint();
   cout<<"\n";

   cout << "HighLevel Manager in an Employee:\n";
   Employee* tmp_emp = hlm;
   (*tmp_emp).print();
   (*tmp_emp).vprint();
   cout << "\n";
   
   cout << "Manager in an Employee:\n";
   tmp_emp = mang;
   (*tmp_emp).print();
   (*tmp_emp).vprint();
   cout << "\n";

   cout << "\tTest Salary methods makes_more1 and makes_more2\n\n";

   Employee charlie("Charlie", 50000.00);
   Employee* dan = new Manager("Danials", 1.00, "here", 0);
   

   charlie.print();
   dan->print();
   
   cout << "\n";

   cout << "makes_more1() in both directions:\n";
   cout << charlie.get_name() << " makes more than " << (*dan).get_name()
        << ": " << charlie.makes_more1(*dan) << "\n";
   cout << (*dan).get_name() << " makes more than " << charlie.get_name()
        << ": " << (*dan).makes_more1(charlie) << "\n";

   cout << "\nmakes_more2() in both directions:\n";
   cout << charlie.get_name() << " makes more than " << (*dan).get_name()
        << ": " << charlie.makes_more2(dan) << "\n";
   cout << (*dan).get_name() << " makes more than " << charlie.get_name()
        << ": " << (*dan).makes_more2(&charlie) << "\n";
}
