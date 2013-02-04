#include <stdlib.h>
#include <stdio.h>
#include "queue.h"

typedef struct {
  int x;
  double y;
} Foo; // Just some arbitrary struct

int main() {
  const int max_entries = 5; // size of queues

  // test
  printf("\ntesting 3 enqueuees, 2 dequeues, and 1 enqueue\n");

  Queue *myqueue = create_queue(max_entries);

  Foo* foo1 = static_cast<Foo *> (malloc(sizeof(Foo)));
  foo1->x = 1;  foo1->y = 1;
  printf("Enqueueing: x = %d, y = %f\n", foo1->x, foo1->y);
  enqueue(myqueue, static_cast<void *> (foo1));

  Foo* foo2 = static_cast<Foo *> (malloc(sizeof(Foo)));
  foo2->x = 2;  foo2->y = 2;
  printf("Enqueueing: x = %d, y = %f\n", foo2->x, foo2->y);
  enqueue(myqueue, static_cast<void *> (foo2));

  Foo* foo3 = static_cast<Foo *> (malloc(sizeof(Foo)));
  foo3->x = 3;  foo3->y = 3;
  printf("Enqueueing: x = %d, y = %f\n", foo3->x, foo3->y);
  enqueue(myqueue, static_cast<void *> (foo3));

  // dequeue twice
  Foo* tmp = static_cast<Foo *> (dequeue(myqueue));
  printf("Dequeueing: x = %d, y = %f\n", tmp->x, tmp->y);

  tmp = static_cast<Foo *> (dequeue(myqueue));
  printf("Dequeueing: x = %d, y = %f\n", tmp->x, tmp->y);

  // enqueue
  printf("Enqueueing: x = %d, y = %f\n", foo3->x, foo3->y);
  enqueue(myqueue, static_cast<void *> (foo3));

  // Clean up
  delete_queue(myqueue);
  free(foo1);
  free(foo2);
  free(foo3);



  // test for overflow
  printf("\nOverflow test\n");


  Queue* overflow = create_queue(max_entries);
  for(int i=0; i<=max_entries+5; i++) {
    tmp = static_cast<Foo *> (malloc(sizeof(Foo)));
    tmp->x = i; tmp->y = i;
    

    if(enqueue(overflow, static_cast<void *> (tmp)) == -1) {
      printf("Queue Overflow!\n");
      break;
    }
    else printf("Enqueueing: x = %d, y = %f\n", tmp->x, tmp->y);
    
  }

  // dequeue everything made off then free it
  for(int i=0; i<=max_entries+5; i++) {
    tmp = static_cast<Foo *> (dequeue(overflow));

    if(tmp == NULL) {
      printf("Cannot keep dequeueping, queue is empty\n");
      break;
    }
    else { 
      printf("Dequeueing: x = %d, y = %f\n", tmp->x, tmp->y);
      free(tmp);
    }
  }
  

  printf("Queue deleted.  Program run successfully\n");
  

  return 0;
}
