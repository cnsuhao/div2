#include <stdlib.h>
#include <stdio.h>
#include "tnode.h"

int main() {
  Tnode* root = NULL;
  root = add_tnode(root, "this ");
  add_tnode(root, "is ");
  add_tnode(root, "a ");
  add_tnode(root, "test ");
  add_tnode(root, "to ");
  add_tnode(root, "SEe ");
  add_tnode(root, "if ");
  add_tnode(root, "sorts ");
  add_tnode(root, "in ");
  add_tnode(root, "the ");
  add_tnode(root, "Proper ");
  add_tnode(root, "Order ");
  add_tnode(root, "! ");
  add_tnode(root, "! ");

  print_tnode(root);
  delete_tnode(root);
  printf("\nTest run successfully.\n");

  return 0;
}
