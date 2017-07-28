#ifndef __REMACS_RUST_TYPES
#define __REMACS_RUST_TYPES

typedef void LispHashTable;

void rust_unmark(void);
void rust_sweep(void);
void rust_mark_hashtable(LispHashTable* table); 

#endif /* __REMACS_RUST_TYPES */
