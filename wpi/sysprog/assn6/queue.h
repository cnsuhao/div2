/** Struct to define a queue; each entry can hold a pointer to anything.
 */
struct queue {
  void **queue_base; // Pointer to base of queue, also the tail
  void **next;  // Pointer to next free cell;
  int max_cells; // Maximum number of entries in the queue
  int cells_used; // Currently used number of cells
};

typedef struct queue Queue;

// Function prototypes

/** Create a queue by allocating a Queue structure, initializing it,
 *  and allocating memory to hold the queue entries.
 * @param max_cells Maximum entries in the queue
 * @return Pointer to newly-allocated Queue structure, NULL if error.
 */
Queue *create_queue(int max_cells);



/** Enqueues a pointer onto a Queue.
 * @param which_queue Pointer to queue you want to add onto.
 * @param ptr Pointer to be added.
 * @return 0 if successful, -1 if not.
 */
int enqueue(Queue *which_queue, void *ptr);

/** Removes value at base of queue and shifts everything down
 * @param which_queue Pointer to Queue you want to dequeue from.
 * @return tail entry of the queue, NULL if queue is empty.
 */
void* dequeue(Queue *which_queue);






/** Deletes a queue, including the structure and the memory
 * for holding the queue entries, but not the entries themselves.
 * @param which_queue Pointer to Queue structure.
 */
void delete_queue(Queue *which_queue);

/** Peek at top of queue, without popping
 * @param which_queue Pointer to Queue you want to peek into
 * @return Top entry of the queue, NULL if queue is empty.
 */
void* peek(Queue *which_queue);
