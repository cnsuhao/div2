/** mystring.cpp
 * My own versions of some of the C-style string functions
*/
#include <string.h>
#include <stdlib.h>
// stdlib.h is needed to use malloc()
#include "mystring.h"
#include "product.h"

#include <iostream>
#include <sys/time.h>

using namespace std;

char* mystrdup(const char* src) {
  int length; // Length of the source string
  char* newstr; // Pointer to memory which will hold new string

  length = mystrlen(src) + 1;  // Leave space for the terminator
  newstr = (char*) malloc(length); // Must cast!

  // If no memory was available, return immediately
  if (newstr == 0) return (char *) 0;

  // Otherwise, copy the string and return pointer to new string
  mystrcpy(newstr, src);
  return newstr;
}

/** Finds the length of a given string
 @param str the string to find the length of
 @return the length of the str
*/
int mystrlen(const char *s) {
  int l = 0;

  const char *tmp = s;

  while(*tmp!=0) {
    tmp++;
    l++;
   }

  return l;
}

/** Copy's given string to another memory allocation
@param s1 location for the copied string
@param s2 location of the given string
@return returns s1
*/
char* mystrcpy(char *restrict s1, const char *restrict s2) {
  const char *restrict src = s2;
  char *restrict dst = s1;

  while(*src != '\0') {
    *dst = *src;
    dst++;
    src++;
  }

  *dst = '\0';

  return s1;
}

/**  Append's a copy of strying pointed to by s2 to the end of s1.
@param s1 the initial string
@param s2 the appended string
@returns returns s1 
*/
char* mystrcat(char *restrict s1, const char *restrict s2) {
  const char *restrict appn = s2;
  char *restrict orig = s1;

  // get the null pointer
  while(*orig != '\0') {
    orig++;
  }

  // append
  while(*appn != '\0') {
    *orig = *appn;
    orig++;
    appn++;
  }

  return s1;
}

/**  Append's n byes of string pointed to by s2 to the end of s1.
@param s1 the initial string
@param s2 the appended string
@returns returns s1 
*/
char* mystrncat(char *restrict s1, const char *restrict s2, size_t n) {
  const char *restrict appn = s2;
  char *restrict orig = s1;

  // get the null pointer
  while(*orig !='\0') {
    orig++;
  }

  // append
  int c = 0;
  while(c<n && *appn != '\0') {
    *orig = *appn;
    orig++;
    appn++;
    c++;
  }
  
  *orig = '\0';

  return s1;
}

/** Copy's given string to another memory allocation for a certain length
@param s1 location for the copied string
@param s2 location of the given string
@return returns s1
*/
char* mystrncpy(char *restrict s1, const char *restrict s2, size_t n) {
  const char *restrict src = s2;
  char *restrict dst = s1;

  int c = 0;
  while(c<n && *src != '\0') {
    *dst = *src;
    dst++;
    src++;
    c++;
  }

  *dst = '\0';

  return s1;
}

/** Duplicates a C-style string.
 @param src Pointer to string to be copied
 @return Pointer to freshly-allocated string containing a duplicate of src
         or null if no memory is available
*/
char* mystrndup(const char* src, size_t n) {
  int length; // Length of the source string
  char* newstr; // Pointer to memory which will hold new string

   newstr = (char*) malloc(n+1); // Must cast!

  // If no memory was available, return immediately
  if (newstr == 0) return (char *) 0;

  // Otherwise, copy the string and return pointer to new string
  mystrncpy(newstr, src, n);
  return newstr;
}

/** Copying a list of Product pointers
    @param src Array of Product pointers
    @param n length of ray being copied
    @return an array of Product pointers
*/
Product** proddup(Product** src, size_t n) {
  Product** newprod = (Product**) calloc(n, sizeof(Product*));

  Product** dst = newprod;
  Product** tmp = src;

  // Otherwise, copy the string and return pointer to new string
  if (newprod == 0) return (Product**) 0;

  for(int i=0; i<n; i++) {
    *dst = *tmp;
    tmp++;
    dst++;    
  }

  return newprod;
}


/** delete array of product pointers
    @param src array of product pointers
    @param n size of array
*/
void proddel(Product** src, size_t n) {
  Product** tmp = src;

  for(int i=0; i<n; i++) {
    delete *tmp;
    tmp++;
  }
}

/** Copying a list of Product pointers with indecies
    @param src Array of Product pointers
    @param n length of ray being copied
    @return an array of Product pointers
*/
Product** prodidup(Product** src, size_t n) {
  Product** newprod = (Product**) calloc(n, sizeof(Product*));
  Product** tmp = src;

  // Otherwise, copy the string and return pointer to new string
  if (newprod == 0) return (Product**) 0;

  for(int i=0; i<n; i++) {
    newprod[i] = tmp[i];   
  }

  return newprod;
}
