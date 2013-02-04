#include <iostream>
#include "product.h"
#include <vector>

int main()
{  
  cout << "\n\nProduct Rater\n\n";
  cout << "Note: Higher score (1-100) or lower price means a higher value\n";
  cout << "\tAlso, the program will read as many products as necessary, there is no \n\tmax.\n\n";
  
  bool more = true;
  vector<Product> products;
  
  // read in the products
  while (more) {  
    // read in next product
    Product next;
    next.read();
    products.push_back(next);
    
    // are there any more products to read?
    cout << "More data? (y/n) ";
    string answer;
    getline(cin, answer);
    if (answer != "y") more = false;
  }
  
  // print
  
  cout << "\n\nOriginal List of Products:\n\n";
  for(int i=0; i<products.size(); i++) {
    products[i].print();
  }
  
  cout << "-----------------------------------------\n";
  cout << "Sorted List of Products:\n\n";
  
  products = Product::sortProducts(products);
  
  for(int i=0; i<products.size(); i++) {
    products[i].print();
  }
  
  return 0;
}
