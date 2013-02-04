#include <iostream>
#include <string.h>
// string.h covers the C-style string functions.
#include <cstdio>
#include "mystring.h"
#include "product.h"
#include <sys/time.h>
using namespace std;

/** ctest.cpp
 * Program to demonstrate character arrays and
 * dynamically-allocated memory.
 * @author Mike Ciaraldi
 */

const int MAX_CHARS = 20; // Maximum number of characters in array
const int MAX_PRODS = 5;  // Maximum number of Products in array

int main()
{
  char a1[MAX_CHARS + 1]; // Character array--initially empty
  char a2[] = "Hello"; // Character array--unsized but initialized
  char a3[MAX_CHARS + 1] = "Hello, also"; // Character array--underfilled
  char* p1 = "Hello"; // Pointer to constant string
  char* p2;           // Will be a pointer to dynamically-allocated string
  int copy_limit;     // Maximum characters to copy.

  Product* prod[5];  // an array of product pointers

  /* Print the pointers.
     We have not discussed printf() yet, but this example prints
     the values of the pointers themselves, not the targets.
  */
  printf("Pointers: a1 = %p, a2 = %p, a3 = %p\n", a1, a2, a3);
  printf("          p1 = %p p2 = %p\n", p1, p2);

  mystrcpy(a1, "Hi"); // Initialize character array

  cout << "a1 = " << a1 << "\n";
  cout << "a2 = " << a2 << "\n";
  cout << "a3 = " << a3 << "\n";

  // Concatenate two character arrays, then print.
  mystrcat(a1, a2);
  cout << "a1 = " << a1 << "\n";

  // Concatenate two character arrays safely, then print.
  copy_limit = MAX_CHARS - mystrlen(a1); // How much space is left?
  if (copy_limit > 0) mystrncat(a1, a2, copy_limit);
  cout << "a1 = " << a1 << "\n";
  // Concatenate two character arrays safely, then print.
  copy_limit = MAX_CHARS - mystrlen(a1); // How much space is left?
  if (copy_limit > 0) mystrncat(a1, a3, copy_limit);
  cout << "a1 = " << a1 << "\n";

  // Duplicate a string, using my function, then print
  printf("Before dup, pointer a2 = %p, contents = %s\n", a2, a2);
  p2 = mystrdup(a2);
  printf("Pointer p2 = %p, contents = %s\n", p2, p2);

  //reset a1;
  mystrcpy(a1,"Hi");
  cout << "\na1 set to: " << a1 << "\n";

  //strncpy
  mystrncpy(a1,a2,4);
  cout << "a1 = " << a1 << "\n";

  //test dupping
  printf("Before strndup 3 bytes, pointer a2 = %p, contents = %s\n", a2, a2);
  char* p3 = mystrndup(a2, 3);
  printf("Pointer p3 = %p, contents = %s\n\n", p3, p3);

  int i;

  for(i=0; i<MAX_PRODS; i++) {
    prod[i] = Product::makeRandomProduct();
  }

  cout << "The pointers of the products in prod: \n";
  for(i=0; i<MAX_PRODS; i++) {
    printf("%p, ", prod[i]);
    prod[i]->print();
  }
  cout << "\n";

  Product** prod2 = proddup(prod, MAX_PRODS);

  cout << "The pointers of the products in prod2: \n";
  for(int j=0; j<MAX_PRODS; j++) {
    printf("%p, ", prod2[j]);
    prod2[j]->print();
  }
  cout << "\n";

  timeval begin;
  timeval end;

  int pointerTime;
  int indexTime;

  // copy with pointers
  gettimeofday(&begin, NULL);
  Product** prod3 = proddup(prod, MAX_PRODS);
  gettimeofday(&end, NULL);
  pointerTime = end.tv_sec*1000 + end.tv_usec - begin.tv_sec*1000 + begin.tv_usec;

  // copy with indecies
  gettimeofday(&begin, NULL);
  Product** prod4 = prodidup(prod, MAX_PRODS);
  gettimeofday(&end, NULL);
  indexTime = end.tv_sec*1000 + end.tv_usec - begin.tv_sec*1000 + begin.tv_usec;

  proddel(prod, MAX_PRODS);

  cout<<"Pointer Time "<<pointerTime<<", Index Time "<< indexTime<<"\n";
  return 0;
}
