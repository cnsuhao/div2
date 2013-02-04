#include <stdlib.h>
#include <cstdlib>
#include <cstdio>
#include <string.h>
#include "tnode.h"

Tnode* add_tnode(Tnode *current_tnode, char* value) {
  // if current_tnode has null for data, allocate and return
  if(current_tnode == NULL) {
    Tnode *new_node;
    new_node = static_cast<Tnode *>(malloc(sizeof(Tnode))); 

    if (new_node == NULL) return NULL; // Error--unable to allocate.

    // Fill in the Node.
    new_node->data = strdup(value);
    new_node->left = NULL;
    new_node->right = NULL;
  
    return new_node;
  }

  // add to left if <= else add to right
  if(strcmp(value, (current_tnode->data)) <= 0) {
    current_tnode->left = add_tnode((current_tnode->left), value);
  }
  else {
     current_tnode->right = add_tnode((current_tnode->right), value);
  }

  return current_tnode;
}

void print_tnode(Tnode *current_tnode) {
  if(current_tnode == NULL) return;

  print_tnode((current_tnode->left));
  printf((current_tnode->data));
  print_tnode((current_tnode->right));
}

void delete_tnode(Tnode *current_tnode) {
  if(current_tnode == NULL) return;

  delete_tnode((current_tnode->left));
  delete_tnode((current_tnode->right));
  free(current_tnode);
}
