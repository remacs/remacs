/* This file contains declarations for elements of xsettings.c that
   were originally static and thus private to that file. Its goal is
   to make these elements (variables functions) acessible when as we
   port xsettings.c to Rust. */

extern char *current_mono_font;
extern char *current_font;
