#ifndef __REMACS_RUST_TYPES
#define __REMACS_RUST_TYPES

typedef void LispHashTable;

void finalize_hashtable(LispHashTable *table);
LispHashTable* purecopy_hash_table(LispHashTable *table);
bool sweep_weak_hashtable(LispHashTable *table, bool remove_entries);
void mark_hashtable(LispHashTable *table);
bool table_not_weak_or_pure(LispHashTable *table);
void sweep_weak_hash_tables(void);

#endif /* __REMACS_RUST_TYPES */
