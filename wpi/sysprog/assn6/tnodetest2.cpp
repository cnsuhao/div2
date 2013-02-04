#include <stdlib.h>
#include <stdio.h>
#include <ctime>
#include "tnode.h"

// generates a random character string of length 'length'
char* generate_str(int length);

int main() {

  const int numchrs = 6;
  const int numwords = 12;

  srand(time(0));

  Tnode* root = NULL;
  
  printf("Tree sort of random string (biggest to smallest): \n");
  
  for(int i=0; i<numwords; i++) {
     root = add_tnode(root, generate_str(numchrs));
  }
  
  print_tnode(root);

  delete_tnode(root);

  printf("\nTest run successfully.\n");

  return 0;
}


char* generate_str(int length) {
  char* newstr = static_cast<char *> (calloc(sizeof(char), length+2));
  char* tmp = newstr;

  for(int i=0; i<length; i++) {
    *tmp = (char)(rand()*26.0/RAND_MAX + 97);
    tmp++;
  }

  *tmp = '\n';
  tmp++;
  tmp = NULL;

  return newstr;
}
