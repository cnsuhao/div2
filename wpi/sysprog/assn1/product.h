#ifndef PRODUCT_H
#define PRODUCT_H

#include <string>
#include <vector>

using namespace std;

class Product
{
public:
   /**
      Constructs a product with score 0 and price 1.
   */
   Product();

   /**
      Reads in this product object.
   */   
   void read();

   /**
      Compares two product objects.
      @param b the object to compare with this object
      @return true if this object is better than b
   */
   bool is_better_than(Product b) const;

   /**
    *  a method to sort a vector of products
    *  @param prods a vector of products
    *  @return returns another vector of sorted products
    */
   static vector<Product> sortProducts(vector<Product> prods);

   /**
      Print this product object
   */
   void print() const;
private:
   string name;
   double price;
   int score;
};

#endif
