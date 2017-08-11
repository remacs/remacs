#ifndef __REMACS_RUST_TYPES
#define __REMACS_RUST_TYPES

typedef void LispHashTable;

void finalize_hashtable(LispHashTable *table);
LispHashTable* pure_copy_hashtable(LispHashTable *table);
bool sweep_weak_hashtable(LispHashTable* table, bool remove_entries);
void hashtable_finalize(LispHashTable* table);

#endif /* __REMACS_RUST_TYPES */
