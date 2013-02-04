/**
 *  A Product Class
 *  @author Alec Goebel
 */

#include <iostream>
#include "product.h"

using namespace std;

/**
 * Product's constructor.  Initializes price and score.
 */
Product::Product()
{  
   price = 1;
   score = 0;
}

/**
 * a function to read the input from the command line
 */
void Product::read()
{  
   cout << "Please enter the model name: ";
   getline(cin, name);
   cout << "Please enter the price: ";
   cin >> price;
   cout << "Please enter the score: ";
   cin >> score;
   string remainder; /* read remainder of line */
   getline(cin, remainder);
}

/**
 * compares two products and returns whether or not given product is better
 * than given product.
 * @param b a given product
 * @return true if given product is better or whichever one is free (since free
 *     is better.)  Note: if both are free than the current best is considered 
 *     better and the function returns false.
 *
 */
bool Product::is_better_than(Product b) const
{  
   if (price == 0 && b.price == 0) return false;
   if (price == 0) return false;
   if (b.price == 0) return true;
   return score / price > b.score / b.price;
}

/**
 * displays the product in the command line
 */
void Product::print() const
{  
   cout << name
      << " Price: " << price
      << " Score: " << score << "\n";
}

/**
 * Sorts a given Product vector.  Based on bubble sort.
 * @param prods a vector of products
 * @return sorted vector of products
 */
vector<Product> Product::sortProducts(vector<Product> prods) {
  while(true) {
    bool inOrder = true;  //assume true until proved otherwise
    
    for(int i=0; i<prods.size()-1;i++) {
      if(prods[i+1].is_better_than(prods[i])) {
	// swap values;
	Product tmp = prods[i];
	prods[i]=prods[i+1];
	prods[i+1]=tmp;

	//wasn't in order
	inOrder = false;
      }
    }

    if(inOrder)
      return prods;
  }  
}

