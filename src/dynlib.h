#ifndef DYNLIB_H
#define DYNLIB_H

#include <config.h>
#include <stdbool.h>

typedef void* dynlib_handle_ptr;
dynlib_handle_ptr dynlib_open (const char * path);
void * dynlib_sym (dynlib_handle_ptr h, const char * sym);
bool dynlib_addr (void *ptr, const char **path, const char **sym);
const char * dynlib_error (void);
int dynlib_close (dynlib_handle_ptr h);

#endif /* DYNLIB_H */
