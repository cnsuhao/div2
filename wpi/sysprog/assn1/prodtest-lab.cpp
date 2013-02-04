#include <iostream>
#include "product.h"

int main()
{  
   Product best;
   Product secondBest;

   bool more = true;
   while (more)
   {  
      Product next;
      next.read();
      if (next.is_better_than(best)) {
	secondBest = best;
	best = next;
      }
      else {
	if(next.is_better_than(secondBest)) {
	  secondBest = next;
	}
      }


      cout << "More data? (y/n) ";
      string answer;
      getline(cin, answer);
      if (answer != "y") more = false;
   }

   cout << "The best value is ";
   best.print();
   cout << "The second best value is ";
   secondBest.print();

   return 0;
}
