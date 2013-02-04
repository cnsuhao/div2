#include <iostream>
#include "product.h"

/* Main program for CS2303 Lab 3
*/

int main()
{  
  const int MAX_PRODUCTS = 5;
  Product a[MAX_PRODUCTS]; // Array of Products;
  Product* b[MAX_PRODUCTS]; // Array of pointers to Products;
  Product c[MAX_PRODUCTS]; // Array if Products;
  Product* d[MAX_PRODUCTS]; // Array of pointers to Products
  Product* newProduct;
 
  int i; // generic loop counter

  // Create an array of random products
  for (i = 0; i < MAX_PRODUCTS; i++) {
    newProduct = Product::makeRandomProduct();
    a[i] = *newProduct; // Generate random Product and copy into array
    delete newProduct;  // then free it
  }

  // Print unsorted array
  cout << "Unsorted array, using objects: \n";
  Product::printArray(a, MAX_PRODUCTS);

  // Print sorted array
  cout << "\nSorted array, using objects: \n";
  Product::sortProducts(a, MAX_PRODUCTS);
  Product::printArray(a, MAX_PRODUCTS);

  // Create an array of pointers to random products
  for (i = 0; i < MAX_PRODUCTS; i++) {
    b[i] = Product::makeRandomProduct();
    // Don't free the objects!
  }

  // Print unsorted array
  cout << "\nUnsorted array, using pointers to objects: \n";
  Product::printArray(b, MAX_PRODUCTS);

  // Print sorted array
  cout << "\nSorted array, using pointers to objects: \n";
  Product::sortProducts(b, MAX_PRODUCTS);
  Product::printArray(b, MAX_PRODUCTS);


  // When all done, free the allocated objects.
  for (i = 0; i < MAX_PRODUCTS; i++) {
    delete b[i];
  }

  // Create an array of random products
  for (i = 0; i < MAX_PRODUCTS; i++) {
    newProduct = Product::makeRandomProduct();
    c[i] = *newProduct; // Generate random Product and copy into array
    delete newProduct;  // then free it
  }

  // last one created on my machine was presorted, so remaking
  for (i = 0; i < MAX_PRODUCTS; i++) {
    newProduct = Product::makeRandomProduct();
    c[i] = *newProduct; // Generate random Product and copy into array
    delete newProduct;  // then free it
  }

  // Print unsorted array
  cout << "\nUnsorted array, using objects: \n";
  Product::printArray(c, MAX_PRODUCTS);

  // Print sorted array
  cout << "\nSorted array, using objects: \n";
  Product::sortPProducts(c, MAX_PRODUCTS);
  Product::printArray(c, MAX_PRODUCTS);

 // Create an array of pointers to random products
  for (i = 0; i < MAX_PRODUCTS; i++) {
    d[i] = Product::makeRandomProduct();
    // Don't free the objects!
  }

  // Print unsorted array
  cout << "\nUnsorted array, using pointers to objects: \n";
  Product::printArray(d, MAX_PRODUCTS);

  // Print sorted array
  cout << "\nSorted array, using pointers to objects: \n";
  Product::sortPProducts(d, MAX_PRODUCTS);
  Product::printArray(d, MAX_PRODUCTS);


  // When all done, free the allocated objects.
  for (i = 0; i < MAX_PRODUCTS; i++) {
    delete d[i];
  }
  return 0;
}
