#include <stdlib.h>

#include "queue.h"

Queue *create_queue(int max_cells) {
  Queue *new_queue; // Holds pointer to the newly-allocated Queue structure.
  new_queue = static_cast<Queue *>(malloc(sizeof(Queue))); 

  if (new_queue == NULL) return NULL; // Error--unable to allocate.

  // Fill in the struct
  new_queue->max_cells = max_cells;
  new_queue->cells_used = 0; // Empty to start

  // Now allocate space for the queue entries.
  new_queue->queue_base = 
    static_cast<void **> (calloc(sizeof(void *), max_cells));
  if (new_queue->queue_base == NULL) {
    free(new_queue); // Unable to allocate queue entries, so free struct.
    return NULL;
  }
  new_queue->next = new_queue->queue_base; // Start at base

  return new_queue;
}

int enqueue(Queue *which_queue, void *ptr) {
  // Check if queue is already full 
  if ((which_queue->cells_used) >= (which_queue->max_cells)) {
    which_queue->cells_used = which_queue->max_cells; // Fix
    return -1;  // Queue overflow.
  }

  // add onto queue.
  *(which_queue->next) = ptr;  // Store the pointer on the queue
  (which_queue->next)++;       // Point to next free cell 
  (which_queue->cells_used)++; 

  return 0;  // Success
}

void* dequeue(Queue *which_queue) {
  // Check if queue is empty
  if ((which_queue->cells_used) <= 0) {
    which_queue->cells_used = 0; // Fix
    return NULL;  // Queue empty
  }
  
  // store tail pointer
  void* returned = *(which_queue->queue_base);
  (which_queue->cells_used)--;

  
  // if empty, you're done
  if((which_queue->cells_used)==0)
    return returned;

  // else shift everything back
  void **tmp = (which_queue->queue_base);
  void **t_next = tmp;
  t_next++;

  while(t_next!=(which_queue->next)) {
    *tmp = *t_next;
    tmp++;
    t_next++;
  }

  (which_queue->next)--;

  return returned;
}




void delete_queue(Queue *which_queue) {
  free(which_queue->queue_base); // Free memory block with queue entries.
  free(which_queue); // Then free the struct.
}
