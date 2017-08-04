#ifndef __REMACS_RUST_TYPES
#define __REMACS_RUST_TYPES

typedef void LispHashTable;

void finalize_hashtable(LispHashTable *table);

#endif /* __REMACS_RUST_TYPES */
