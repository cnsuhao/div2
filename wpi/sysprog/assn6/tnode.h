/** Struct to define a stack; each entry can hold a pointer to anything.
 */
struct tnode {
  char *data; // Pointer to data (c-style string)
  tnode *left; // Pointer to location of left tnode
  tnode *right; // Pointer to location of right tnode
};

typedef struct tnode Tnode;

/** Add a tnode to the current list of tnodes
    @param current_tnode the tnode to add new value to
    @param value the string to add as data to the next node
    @return pointer to the current node (if node was null, the new node)
**/
Tnode* add_tnode(Tnode *current_tnode, char* value);

/** Print the tnode INORDER (left, self, right)
    @param current_tnode the tnode to add new value to
**/
void print_tnode(Tnode *current_tnode);

/** delete all nodes in the tree, left then right
    @param current_tnode the tnode to add new value to
**/
void delete_tnode(Tnode *current_tnode);
