/* JSON parsing and serialization.

Copyright (C) 2017 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

#include <config.h>

#include <errno.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

#include <jansson.h>

#include "lisp.h"
#include "buffer.h"
#include "coding.h"

/* We install a custom allocator so that we can avoid objects larger
   than PTRDIFF_MAX.  Such objects wouldn’t play well with the rest of
   Emacs’s codebase, which generally uses ptrdiff_t for sizes and
   indices.  The other functions in this file also generally assume
   that size_t values never exceed PTRDIFF_MAX.  */

static void *
json_malloc (size_t size)
{
  if (size > PTRDIFF_MAX)
    {
      errno = ENOMEM;
      return NULL;
    }
  return malloc (size);
}

static void
json_free (void *ptr)
{
  free (ptr);
}

void
init_json (void)
{
  json_set_alloc_funcs (json_malloc, json_free);
}

/* Return whether STRING starts with PREFIX.  */

static bool
json_has_prefix (const char *string, const char *prefix)
{
  size_t string_len = strlen (string);
  size_t prefix_len = strlen (prefix);
  return string_len >= prefix_len && memcmp (string, prefix, prefix_len) == 0;
}

/* Return whether STRING ends with SUFFIX.  */

static bool
json_has_suffix (const char *string, const char *suffix)
{
  size_t string_len = strlen (string);
  size_t suffix_len = strlen (suffix);
  return string_len >= suffix_len
    && memcmp (string + string_len - suffix_len, suffix, suffix_len) == 0;
}

/* Create a multibyte Lisp string from the UTF-8 string in
   [DATA, DATA + SIZE).  If the range [DATA, DATA + SIZE) does not
   contain a valid UTF-8 string, an unspecified string is
   returned.  */

static Lisp_Object
json_make_string (const char *data, ptrdiff_t size)
{
  return code_convert_string (make_specified_string (data, -1, size, false),
                              Qutf_8_unix, Qt, false, true, true);
}

/* Create a multibyte Lisp string from the null-terminated UTF-8
   string beginning at DATA.  If the string is not a valid UTF-8
   string, an unspecified string is returned.  */

static Lisp_Object
json_build_string (const char *data)
{
  return json_make_string (data, strlen (data));
}

/* Return a unibyte string containing the sequence of UTF-8 encoding
   units of the UTF-8 representation of STRING.  If STRING does not
   represent a sequence of Unicode scalar values, return a string with
   unspecified contents.  */

static Lisp_Object
json_encode (Lisp_Object string)
{
  return code_convert_string (string, Qutf_8_unix, Qt, true, true, true);
}

static _Noreturn void
json_out_of_memory (void)
{
  xsignal0 (Qjson_out_of_memory);
}

/* Signal a Lisp error corresponding to the JSON ERROR.  */

static _Noreturn void
json_parse_error (const json_error_t *error)
{
  Lisp_Object symbol;
  /* FIXME: Upstream Jansson should have a way to return error codes
     without parsing the error messages.  See
     https://github.com/akheron/jansson/issues/352.  */
  if (json_has_suffix (error->text, "expected near end of file"))
    symbol = Qjson_end_of_file;
  else if (json_has_prefix (error->text, "end of file expected"))
    symbol = Qjson_trailing_content;
  else
    symbol = Qjson_parse_error;
  xsignal (symbol,
           list5 (json_build_string (error->text),
                  json_build_string (error->source), make_natnum (error->line),
                  make_natnum (error->column), make_natnum (error->position)));
}

static void
json_release_object (void *object)
{
  json_decref (object);
}

/* Signal an error if OBJECT is not a string, or if OBJECT contains
   embedded null characters.  */

static void
check_string_without_embedded_nulls (Lisp_Object object)
{
  CHECK_STRING (object);
  CHECK_TYPE (memchr (SDATA (object), '\0', SBYTES (object)) == NULL,
              Qstring_without_embedded_nulls_p, object);
}

/* Signal an error of type `json-out-of-memory' if OBJECT is
   NULL.  */

static json_t *
json_check (json_t *object)
{
  if (object == NULL)
    json_out_of_memory ();
  return object;
}

static json_t *lisp_to_json (Lisp_Object);

/* Convert a Lisp object to a toplevel JSON object (array or object).
   This returns Lisp_Object so we can use unbind_to.  The return value
   is always nil.  */

static _GL_ARG_NONNULL ((2)) Lisp_Object
lisp_to_json_toplevel_1 (Lisp_Object lisp, json_t **json)
{
  if (VECTORP (lisp))
    {
      ptrdiff_t size = ASIZE (lisp);
      *json = json_check (json_array ());
      ptrdiff_t count = SPECPDL_INDEX ();
      record_unwind_protect_ptr (json_release_object, json);
      for (ptrdiff_t i = 0; i < size; ++i)
        {
          int status
            = json_array_append_new (*json, lisp_to_json (AREF (lisp, i)));
          if (status == -1)
            json_out_of_memory ();
        }
      eassert (json_array_size (*json) == size);
      clear_unwind_protect (count);
      return unbind_to (count, Qnil);
    }
  else if (HASH_TABLE_P (lisp))
    {
      struct Lisp_Hash_Table *h = XHASH_TABLE (lisp);
      *json = json_check (json_object ());
      ptrdiff_t count = SPECPDL_INDEX ();
      record_unwind_protect_ptr (json_release_object, *json);
      for (ptrdiff_t i = 0; i < HASH_TABLE_SIZE (h); ++i)
        if (!NILP (HASH_HASH (h, i)))
          {
            Lisp_Object key = json_encode (HASH_KEY (h, i));
            /* We can’t specify the length, so the string must be
               null-terminated.  */
            check_string_without_embedded_nulls (key);
            int status = json_object_set_new (*json, SSDATA (key),
                                              lisp_to_json (HASH_VALUE (h, i)));
            if (status == -1)
              json_out_of_memory ();
          }
      clear_unwind_protect (count);
      return unbind_to (count, Qnil);
    }
  wrong_type_argument (Qjson_value_p, lisp);
}

/* Convert LISP to a toplevel JSON object (array or object).  Signal
   an error of type `wrong-type-argument' if LISP is not a vector or
   hashtable.  */

static json_t *
lisp_to_json_toplevel (Lisp_Object lisp)
{
  if (++lisp_eval_depth > max_lisp_eval_depth)
    xsignal0 (Qjson_object_too_deep);
  json_t *json;
  lisp_to_json_toplevel_1 (lisp, &json);
  --lisp_eval_depth;
  return json;
}

/* Convert LISP to any JSON object.  Signal an error of type
   `wrong-type-argument' if the type of LISP can't be converted to a
   JSON object.  */

static json_t *
lisp_to_json (Lisp_Object lisp)
{
  if (EQ (lisp, QCnull))
    return json_check (json_null ());
  else if (EQ (lisp, QCfalse))
    return json_check (json_false ());
  else if (EQ (lisp, Qt))
    return json_check (json_true ());
  else if (INTEGERP (lisp))
    {
      CHECK_TYPE_RANGED_INTEGER (json_int_t, lisp);
      return json_check (json_integer (XINT (lisp)));
    }
  else if (FLOATP (lisp))
    return json_check (json_real (XFLOAT_DATA (lisp)));
  else if (STRINGP (lisp))
    {
      Lisp_Object encoded = json_encode (lisp);
      ptrdiff_t size = SBYTES (encoded);
      return json_check (json_stringn (SSDATA (encoded), size));
    }

  /* LISP now must be a vector or hashtable.  */
  return lisp_to_json_toplevel (lisp);
}

DEFUN ("json-serialize", Fjson_serialize, Sjson_serialize, 1, 1, NULL,
       doc: /* Return the JSON representation of OBJECT as a string.
OBJECT must be a vector or hashtable, and its elements can recursively
contain `:null', `:false', t, numbers, strings, or other vectors and
hashtables.  `:null', `:false', and t will be converted to JSON null,
false, and true values, respectively.  Vectors will be converted to
JSON arrays, and hashtables to JSON objects.  Hashtable keys must be
strings without embedded null characters and must be unique within
each object.  */)
  (Lisp_Object object)
{
  ptrdiff_t count = SPECPDL_INDEX ();

  json_t *json = lisp_to_json_toplevel (object);
  record_unwind_protect_ptr (json_release_object, json);

  char *string = json_dumps (json, JSON_COMPACT);
  if (string == NULL)
    json_out_of_memory ();
  record_unwind_protect_ptr (free, string);

  return unbind_to (count, json_build_string (string));
}

struct json_buffer_and_size
{
  const char *buffer;
  ptrdiff_t size;
};

static Lisp_Object
json_insert (void *data)
{
  struct json_buffer_and_size *buffer_and_size = data;
  /* FIXME: This should be possible without creating an intermediate
     string object.  */
  Lisp_Object string
    = json_make_string (buffer_and_size->buffer, buffer_and_size->size);
  insert1 (string);
  return Qnil;
}

struct json_insert_data
{
  /* nil if json_insert succeeded, otherwise the symbol
     Qcatch_all_memory_full or a cons (ERROR-SYMBOL . ERROR-DATA).  */
  Lisp_Object error;
};

/* Callback for json_dump_callback that inserts the UTF-8 string in
   [BUFFER, BUFFER + SIZE) into the current buffer.
   If [BUFFER, BUFFER + SIZE) does not contain a valid UTF-8 string,
   an unspecified string is inserted into the buffer.  DATA must point
   to a structure of type json_insert_data.  This function may not
   exit nonlocally.  It catches all nonlocal exits and stores them in
   data->error for reraising.  */

static int
json_insert_callback (const char *buffer, size_t size, void *data)
{
  struct json_insert_data *d = data;
  struct json_buffer_and_size buffer_and_size
    = {.buffer = buffer, .size = size};
  d->error = internal_catch_all (json_insert, &buffer_and_size, Fidentity);
  return NILP (d->error) ? 0 : -1;
}

DEFUN ("json-insert", Fjson_insert, Sjson_insert, 1, 1, NULL,
       doc: /* Insert the JSON representation of OBJECT before point.
This is the same as (insert (json-serialize OBJECT)), but potentially
faster.  See the function `json-serialize' for allowed values of
OBJECT.  */)
  (Lisp_Object object)
{
  ptrdiff_t count = SPECPDL_INDEX ();

  json_t *json = lisp_to_json (object);
  record_unwind_protect_ptr (json_release_object, json);

  struct json_insert_data data;
  int status
    = json_dump_callback (json, json_insert_callback, &data, JSON_COMPACT);
  if (status == -1)
    {
      if (CONSP (data.error))
        xsignal (XCAR (data.error), XCDR (data.error));
      else
        json_out_of_memory ();
    }

  return unbind_to (count, Qnil);
}

/* Convert a JSON object to a Lisp object.  */

static _GL_ARG_NONNULL ((1)) Lisp_Object
json_to_lisp (json_t *json)
{
  switch (json_typeof (json))
    {
    case JSON_NULL:
      return QCnull;
    case JSON_FALSE:
      return QCfalse;
    case JSON_TRUE:
      return Qt;
    case JSON_INTEGER:
      /* Return an integer if possible, a floating-point number
         otherwise.  This loses precision for integers with large
         magnitude; however, such integers tend to be nonportable
         anyway because many JSON implementations use only 64-bit
         floating-point numbers with 53 mantissa bits.  See
         https://tools.ietf.org/html/rfc7159#section-6 for some
         discussion.  */
      return make_fixnum_or_float (json_integer_value (json));
    case JSON_REAL:
      return make_float (json_real_value (json));
    case JSON_STRING:
      return json_make_string (json_string_value (json),
                               json_string_length (json));
    case JSON_ARRAY:
      {
        if (++lisp_eval_depth > max_lisp_eval_depth)
          xsignal0 (Qjson_object_too_deep);
        size_t size = json_array_size (json);
        if (FIXNUM_OVERFLOW_P (size))
          xsignal0 (Qoverflow_error);
        Lisp_Object result = Fmake_vector (make_natnum (size), Qunbound);
        for (ptrdiff_t i = 0; i < size; ++i)
          ASET (result, i,
                json_to_lisp (json_array_get (json, i)));
        --lisp_eval_depth;
        return result;
      }
    case JSON_OBJECT:
      {
        if (++lisp_eval_depth > max_lisp_eval_depth)
          xsignal0 (Qjson_object_too_deep);
        size_t size = json_object_size (json);
        if (FIXNUM_OVERFLOW_P (size))
          xsignal0 (Qoverflow_error);
        Lisp_Object result = CALLN (Fmake_hash_table, QCtest, Qequal,
                                    QCsize, make_natnum (size));
        struct Lisp_Hash_Table *h = XHASH_TABLE (result);
        const char *key_str;
        json_t *value;
        json_object_foreach (json, key_str, value)
          {
            Lisp_Object key = json_build_string (key_str);
            EMACS_UINT hash;
            ptrdiff_t i = hash_lookup (h, key, &hash);
            /* Keys in JSON objects are unique, so the key can’t be
               present yet.  */
            eassert (i < 0);
            hash_put (h, key, json_to_lisp (value), hash);
          }
        --lisp_eval_depth;
        return result;
      }
    }
  /* Can’t get here.  */
  emacs_abort ();
}

DEFUN ("json-parse-string", Fjson_parse_string, Sjson_parse_string, 1, 1, NULL,
       doc: /* Parse the JSON STRING into a Lisp object.
This is essentially the reverse operation of `json-serialize', which
see.  The returned object will be a vector or hashtable.  Its elements
will be `:null', `:false', t, numbers, strings, or further vectors and
hashtables.  If there are duplicate keys in an object, all but the
last one are ignored.  If STRING doesn't contain a valid JSON object,
an error of type `json-parse-error' is signaled.  */)
  (Lisp_Object string)
{
  ptrdiff_t count = SPECPDL_INDEX ();
  Lisp_Object encoded = json_encode (string);
  check_string_without_embedded_nulls (encoded);

  json_error_t error;
  json_t *object = json_loads (SSDATA (encoded), 0, &error);
  if (object == NULL)
    json_parse_error (&error);

  /* Avoid leaking the object in case of further errors.  */
  if (object != NULL)
    record_unwind_protect_ptr (json_release_object, object);

  return unbind_to (count, json_to_lisp (object));
}

struct json_read_buffer_data
{
  /* Byte position of position to read the next chunk from.  */
  ptrdiff_t point;
};

/* Callback for json_load_callback that reads from the current buffer.
   DATA must point to a structure of type json_read_buffer_data.
   data->point must point to the byte position to read from; after
   reading, data->point is advanced accordingly.  The buffer point
   itself is ignored.  This function may not exit nonlocally.  */

static size_t
json_read_buffer_callback (void *buffer, size_t buflen, void *data)
{
  struct json_read_buffer_data *d = data;

  /* First, parse from point to the gap or the end of the accessible
     portion, whatever is closer.  */
  ptrdiff_t point = d->point;
  ptrdiff_t end = BUFFER_CEILING_OF (point) + 1;
  ptrdiff_t count = end - point;
  if (buflen < count)
    count = buflen;
  memcpy (buffer, BYTE_POS_ADDR (point), count);
  d->point += count;
  return count;
}

DEFUN ("json-parse-buffer", Fjson_parse_buffer, Sjson_parse_buffer,
       0, 0, NULL,
       doc: /* Read JSON object from current buffer starting at point.
This is similar to `json-parse-string', which see.  Move point after
the end of the object if parsing was successful.  On error, point is
not moved.  */)
  (void)
{
  ptrdiff_t count = SPECPDL_INDEX ();

  ptrdiff_t point = PT_BYTE;
  struct json_read_buffer_data data = {.point = point};
  json_error_t error;
  json_t *object = json_load_callback (json_read_buffer_callback, &data,
                                       JSON_DISABLE_EOF_CHECK, &error);

  if (object == NULL)
    json_parse_error (&error);

  /* Avoid leaking the object in case of further errors.  */
  record_unwind_protect_ptr (json_release_object, object);

  /* Convert and then move point only if everything succeeded.  */
  Lisp_Object lisp = json_to_lisp (object);

  /* Adjust point by how much we just read.  */
  point += error.position;
  SET_PT_BOTH (BYTE_TO_CHAR (point), point);

  return unbind_to (count, lisp);
}

/* Simplified version of ‘define-error’ that works with pure
   objects.  */

static void
define_error (Lisp_Object name, const char *message, Lisp_Object parent)
{
  eassert (SYMBOLP (name));
  eassert (SYMBOLP (parent));
  Lisp_Object parent_conditions = Fget (parent, Qerror_conditions);
  eassert (CONSP (parent_conditions));
  eassert (!NILP (Fmemq (parent, parent_conditions)));
  eassert (NILP (Fmemq (name, parent_conditions)));
  Fput (name, Qerror_conditions, pure_cons (name, parent_conditions));
  Fput (name, Qerror_message, build_pure_c_string (message));
}

void
syms_of_json (void)
{
  DEFSYM (QCnull, ":null");
  DEFSYM (QCfalse, ":false");

  DEFSYM (Qstring_without_embedded_nulls_p, "string-without-embedded-nulls-p");
  DEFSYM (Qjson_value_p, "json-value-p");

  DEFSYM (Qutf_8_unix, "utf-8-unix");

  DEFSYM (Qjson_error, "json-error");
  DEFSYM (Qjson_out_of_memory, "json-out-of-memory");
  DEFSYM (Qjson_parse_error, "json-parse-error");
  DEFSYM (Qjson_end_of_file, "json-end-of-file");
  DEFSYM (Qjson_trailing_content, "json-trailing-content");
  DEFSYM (Qjson_object_too_deep, "json-object-too-deep");
  define_error (Qjson_error, "generic JSON error", Qerror);
  define_error (Qjson_out_of_memory,
                "not enough memory for creating JSON object", Qjson_error);
  define_error (Qjson_parse_error, "could not parse JSON stream",
                Qjson_error);
  define_error (Qjson_end_of_file, "end of JSON stream", Qjson_parse_error);
  define_error (Qjson_trailing_content, "trailing content after JSON stream",
                Qjson_parse_error);
  define_error (Qjson_object_too_deep,
                "object cyclic or Lisp evaluation too deep", Qjson_error);

  DEFSYM (Qpure, "pure");
  DEFSYM (Qside_effect_free, "side-effect-free");

  DEFSYM (Qjson_serialize, "json-serialize");
  DEFSYM (Qjson_parse_string, "json-parse-string");
  Fput (Qjson_serialize, Qpure, Qt);
  Fput (Qjson_serialize, Qside_effect_free, Qt);
  Fput (Qjson_parse_string, Qpure, Qt);
  Fput (Qjson_parse_string, Qside_effect_free, Qt);

  defsubr (&Sjson_serialize);
  defsubr (&Sjson_insert);
  defsubr (&Sjson_parse_string);
  defsubr (&Sjson_parse_buffer);
}
