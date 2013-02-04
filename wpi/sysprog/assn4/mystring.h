/** mystring.h
 * My own versions of some of the C-style string functions
*/

#include "product.h"

#ifdef __cplusplus
#define restrict
#endif

/** Duplicates a C-style string.
 @param src Pointer to string to be copied
 @return Pointer to freshly-allocated string containing a duplicate of src
         or null if no memory is available
*/
char* mystrdup(const char* src);

/** Finds the length of a given string
 @param str the string to find the length of
 @return the length of the str
*/
int mystrlen(const char *s);

/** Copy's given string to another memory allocation
@param s1 location for the copied string
@param s2 location of the given string
@return returns s1
*/
char* mystrcpy(char *restrict s1, const char *restrict s2);

/**  Append's a copy of strying pointed to by s2 to the end of s1.
@param s1 the initial string
@param s2 the appended string
@returns returns s1 
*/
char* mystrcat(char *restrict s1, const char *restrict s2);

/**  Append's n byes of string pointed to by s2 to the end of s1.
@param s1 the initial string
@param s2 the appended string
@param n number of bytes dupped
@returns returns s1 
*/
char* mystrncat(char *restrict s1, const char *restrict s2, size_t n);

/** Copy's given string to another memory allocation for a certain length
@param s1 location for the copied string
@param s2 location of the given string
@param n number of bytes dupped
@return returns s1
*/
char* mystrncpy(char *restrict s1, const char *restrict s2, size_t n);

/** Duplicates a C-style string n byes
 @param src Pointer to string to be copied
 @param n number of bytes dupped
 @return Pointer to freshly-allocated string containing a duplicate of src
         or null if no memory is available
*/
char* mystrndup(const char* src, size_t n);

/** Copying a list of Product pointers
    @param src Array of Product pointers
    @param n length of ray being copied
    @return an array of Product pointers
*/
Product** proddup(Product** src, size_t n);

/** Copying a list of Products
    @param src Array of Products
    @param n length of ray being copied
    @return an array of Product pointers
*/
Product* proddup2(Product* src, size_t n);

/** Delete array of product pointers
    @param src array of product pointers
    @param n length of array being deleted
*/
void proddel(Product** src, size_t n);

/** Copying a list of Product pointers with indecies
    @param src Array of Product pointers
    @param n length of ray being copied
    @return an array of Product pointers
*/
Product** prodidup(Product** src, size_t n);
