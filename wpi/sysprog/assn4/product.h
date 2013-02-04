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
     Constructs a product with specified score and price.
     @param newPrice price for the new object, in US Dollars
     @param newScore score for the new object, scale 0-100.
  */
  Product(double newPrice, int newScore);

 /**
     Constructs a product with specified score and price.
     @param name the products name
     @param newPrice price for the new object, in US Dollars
     @param newScore score for the new object, scale 0-100.
  */
  Product(string newName, double newPrice, int newScore);

  /**
     Reads in this product object.
  */   
  void read();

  /**
     Compares two product objects.
     @param b the object to compare with this object
     @return true if this object is strictly better than b,
     based on ratio of score to price. (in case of tie, 
     returns false).
  */
  bool is_better_than(Product b) const;

  /**
     Print this product object
  */
  void print() const;

  /**
     Instantiates a product and fills it with random data
     @return Pointer to a new Product
  */
  static Product* makeRandomProduct();

  /**
     Instantiates a product and fills it with random data
     @param max_price max value for price
     @param max_score max value for score
     @return Pointer to a new Product
  */
  static Product* makeRandomProduct(double max_price, double max_score, int length);

  /** 
      Prints an array of Products
      @ar Array of Products
      @size Number of elements in the array
  */
  static void printArray(Product ar[], int size);

  /** 
      Prints an array of Products
      @ar Array of pointers to Products
      @size Number of elements in the array
  */
  static void printArray(Product* ar[], int size);
 
   /**
    *  a method to sort a vector of products
    *  @param prods a vector of products
    *  @return returns another vector of sorted products
    */
   static vector<Product> sortProducts(vector<Product> prods);

   /**
    *  a method to sort an array of products
    *  @param prods an array of products
    */
   static void sortProducts(Product prods[], int size);

   /**
    *  a method to sort an array of products
    *  @param prods an array of products
    */
   static void sortProducts(Product* prods[], int size);

 /**
    *  a method to sort an array of products
    *  @param prods an array of products
    */
   static void sortPProducts(Product prods[], int size);

 /**
    *  a method to sort an array of product pointers
    *  @param prods an array of pointers
    */
   static void sortPProducts(Product* prods[], int size);

 private:
  string name;
  double price; // Price of the product in US Dollars
  int score; // Score on a scale of 0 to 100.
};

#endif
