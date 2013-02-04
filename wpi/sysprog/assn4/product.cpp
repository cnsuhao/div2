#include <iostream>
#include <cstdlib>
#include "product.h"

using namespace std;

Product::Product()
{  
  price = 1;
  score = 0;
}

Product::Product(double newPrice, int newScore)
{  
  price = newPrice;
  score = newScore;
}

Product::Product(string newName, double newPrice, int newScore)
{ 
  name = newName;
  price = newPrice;
  score = newScore;
}

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

bool Product::is_better_than(Product b) const
{  
  // If both products are free, higher score is better
  if (price == 0 && b.price == 0) return (score > b.score);
  // If only one product is free, it is better. 
  if (price == 0) return true;
  if (b.price == 0) return false;
  // Otherwise, compare ratio of score to price.
  return ((score / price) > (b.score / b.price));
}

void Product::print() const
{ 
  string display = name;
  if(name=="") display = "NO NAME";

  cout << display << ", Score: " << score << ", Price: " << price;

  if(price==0) cout << ", Value: Infinite\n";
  else cout << ", Value: " << score/price << "\n";
}

Product* Product::makeRandomProduct()
{
  double newPrice;
  int newScore; // Randomly generated price and score 
  int length = 5;

  // Standard function rand() returns an integer between 
  // zero and RAND_MAX.

  // Note: RAND_MAX might be equal to the largest integer, so 
  // be sure to use a double divide, not an integer divide.
  newPrice = (100.0 * rand()) / RAND_MAX;
  newScore = (int) ((100.0 * rand()) / RAND_MAX);

  string name = "";

  for(int i=0; i<length; i++) {
    name += (char)((rand()*26.0)/RAND_MAX + 65);
  }

  Product* newProduct = new Product(name, newPrice, newScore);

  return(newProduct);
}

Product* Product::makeRandomProduct(double max_price, double max_score, int length)
{
  double newPrice;
  int newScore; // Randomly generated price and score 

  // Standard function rand() returns an integer between 
  // zero and RAND_MAX.

  // Note: RAND_MAX might be equal to the largest integer, so 
  // be sure to use a double divide, not an integer divide.
  newPrice = (max_price*rand())/RAND_MAX;
  newScore = (int) ((rand()*max_score)/RAND_MAX);

  string name = "";

  for(int i=0; i<length; i++) {
    name += (char)((int)((rand()*26.0)/RAND_MAX + 65));
  }

  Product* newProduct = new Product(name, newPrice, newScore);

  return(newProduct);
}

void Product::printArray(Product ar[], int size) {
  int i; // loop counter

  for (i = 0; i < size; i++) {
    ar[i].print();
  }
} 

void Product::printArray(Product* ar[], int size) {
  int i; // loop counter

  for (i = 0; i < size; i++) {
    ar[i]->print();
  }
} 

/**
 * Sorts a given Product vector.  Based on bubble sort.
 * @param prods a vector of products
 * @return sorted vector of products
 */
void Product::sortProducts(Product prods[], int size) {
  for(int j=size-1; j>0; j--) {
    for(int i=0; i<j;i++) {
      if(prods[i+1].is_better_than(prods[i])) {
	// swap values;
	Product tmp = prods[i];
	prods[i]=prods[i+1];
	prods[i+1]=tmp;
      }
    }
  }
}  

/**
 * Sorts a given Product vector.  Based on bubble sort.
 * @param prods a vector of products
 * @return sorted vector of products
 */
void Product::sortProducts(Product* prods[], int size) {
  for(int j=size-1; j>0; j--) {
    for(int i=0; i<j;i++) {
      if((*prods[i+1]).is_better_than((*prods[i]))) {
	// swap values;
	Product* tmp = prods[i];
	prods[i]=prods[i+1];
	prods[i+1]=tmp;
      }
    }
  }
}  

/**
 * Sorts a given Product vector.  Based on bubble sort.
 * @param prods a vector of products
 * @return sorted vector of products
 */
vector<Product> Product::sortProducts(vector<Product> prods) {
  for(int j=prods.size()-1; j>0; j--) {
    for(int i=0; i<j;i++) {
      if(prods[i+1].is_better_than(prods[i])) {
	// swap values;
	Product tmp = prods[i];
	prods[i]=prods[i+1];
	prods[i+1]=tmp;
      }
    }
  }
}  

/**
 * Sorts a given Product array with pointers.  Based on bubble sort.
 * @param prods an array of products
 */
void Product::sortPProducts(Product prods[], int size) {
  Product* p = prods;
  for(int j=size-1; j>0; j--) {
    for(int i=0; i<j;i++) {
      Product* curr = p;
      p++;
      Product* next = p;
      if(!curr->is_better_than(*next)) {

	// swap values;
	Product tmp = *curr;
	*curr = *next;
	*next = tmp;
      }
    }
    p = prods;
  }
}

/**
 * Sorts a given Product array of pointers.  Based on bubble sort.
 * @param prods an array of pointers
 */
void Product::sortPProducts(Product* prods[], int size) {
  Product** p = prods;
  for(int j=size-1; j>0; j--) {
    for(int i=0; i<j;i++) {
      Product** curr = p;
      p++;
      Product** next = p;
      if(!(**curr).is_better_than(**next)) {

	// swap values;
	Product* tmp = *curr;
	*curr = *next;
	*next = tmp;
      }
    }
    p = prods;
  }
}
