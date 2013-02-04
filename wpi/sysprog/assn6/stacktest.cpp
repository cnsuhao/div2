#include <stdlib.h>
#include <stdio.h>
#include "stack.h"

typedef struct {
  int x;
  double y;
} Foo; // Just some arbitrary struct


int main() {
  const int max_entries = 5; // size of stacks
  Foo* new_foo1;
  Foo* new_foo2; 
  Foo* returned_foo;

  // First, create a stack
  Stack *new_stack = create_stack(max_entries);

  // Allocate a Foo and push it onto the stack.
  new_foo1 = static_cast<Foo *> (malloc(sizeof(Foo)));
  new_foo1->x = 5;
  new_foo1->y = 10.78;
  printf("Pushing: x = %5d, y = %10.3f\n", new_foo1->x, new_foo1->y);
  push(new_stack, static_cast<void *> (new_foo1));

  // Allocate another Foo and push it onto the stack.
  new_foo2 = static_cast<Foo *> (malloc(sizeof(Foo)));
  new_foo2->x = 27;
  new_foo2->y = 3.14159;
  printf("Pushing: x = %5d, y = %10.3f\n", new_foo2->x, new_foo2->y);
  push(new_stack, static_cast<void *> (new_foo2));

  // Retrieve the Foos and print them.
  returned_foo = static_cast<Foo *> (pop(new_stack));
  printf("Poped:  x = %5d, y = %10.3f\n", returned_foo->x, returned_foo->y);
  returned_foo = static_cast<Foo *> (pop(new_stack));
  printf("Poped:  x = %5d, y = %10.3f\n", returned_foo->x, returned_foo->y);

  // test 2
  printf("\ntesting 3 pushes, 2 pops, and 1 push\n");

  Stack *mystack = create_stack(max_entries);

  Foo* foo1 = static_cast<Foo *> (malloc(sizeof(Foo)));
  foo1->x = 1;  foo1->y = 1;
  printf("Pushing: x = %d, y = %f\n", foo1->x, foo1->y);
  push(mystack, static_cast<void *> (foo1));

  Foo* foo2 = static_cast<Foo *> (malloc(sizeof(Foo)));
  foo2->x = 2;  foo2->y = 2;
  printf("Pushing: x = %d, y = %f\n", foo2->x, foo2->y);
  push(mystack, static_cast<void *> (foo2));

  Foo* foo3 = static_cast<Foo *> (malloc(sizeof(Foo)));
  foo3->x = 3;  foo3->y = 3;
  printf("Pushing: x = %d, y = %f\n", foo3->x, foo3->y);
  push(mystack, static_cast<void *> (foo3));

  // pop twice and peek
  Foo* tmp = static_cast<Foo *> (pop(mystack));
  printf("Popping: x = %d, y = %f\n", tmp->x, tmp->y);

  tmp = static_cast<Foo *> (pop(mystack));
  printf("Popping: x = %d, y = %f\n", tmp->x, tmp->y);

  tmp = static_cast<Foo *> (peek(mystack));
  printf("Peeking: x = %d, y = %f\n", tmp->x, tmp->y);

  // push and peek
  printf("Pushing: x = %d, y = %f\n", foo3->x, foo3->y);
  push(mystack, static_cast<void *> (foo3));
  tmp = static_cast<Foo *> (peek(mystack));
  printf("Peeking: x = %d, y = %f\n", tmp->x, tmp->y);

  // Clean up
  delete_stack(mystack);
  free(foo1);
  free(foo2);
  free(foo3);



  // test for overflow
  printf("\nOverflow test\n");


  Stack* overflow = create_stack(max_entries);
  for(int i=0; i<=max_entries+5; i++) {
    tmp = static_cast<Foo *> (malloc(sizeof(Foo)));
    tmp->x = i; tmp->y = i;
    

    if(push(overflow, static_cast<void *> (tmp)) == -1) {
      printf("Stack Overflow!\n");
      break;
    }
    else printf("Pushing: x = %d, y = %f\n", tmp->x, tmp->y);
    
  }

  // pop everything made off then free it
  for(int i=0; i<=max_entries+5; i++) {
    tmp = static_cast<Foo *> (pop(overflow));

    if(tmp == NULL) {
      printf("Cannot keep popping, stack is empty\n");
      break;
    }
    else { 
      printf("Popping: x = %d, y = %f\n", tmp->x, tmp->y);
      free(tmp);
    }
  }
  

  printf("Stack deleted.  Program run successfully\n");
  

  return 0;
}
