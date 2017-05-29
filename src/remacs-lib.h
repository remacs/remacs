// This file defines the C API for Rust functions defined in
// the remacs-lib crate.

#ifndef _REMACS_LIB_H
#define _REMACS_LIB_H

// This function is a rust implementation of mkostemp.
// It should match the API of that function, and make
// the same guarantees
int rust_make_temp(char *template, int flags);

int rust_count_trailing_zeros(unsigned int x);
int rust_count_trailing_zeros_l(unsigned long x);
int rust_count_trailing_zeros_ll(unsigned long long x);

int rust_count_one_bits(unsigned int x);
int rust_count_one_bits_l(unsigned long x);
int rust_count_one_bits_ll(unsigned long long x);

#endif
