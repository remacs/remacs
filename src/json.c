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

#include <stddef.h>
#include <stdint.h>

#include <jansson.h>

#include "lisp.h"
#include "buffer.h"

static _Noreturn void
json_out_of_memory (void)
{
  xsignal0 (Qjson_out_of_memory);
}

static _Noreturn void
json_parse_error (const json_error_t *error)
{
  xsignal (Qjson_parse_error,
           list5 (build_string (error->text), build_string (error->source),
                  make_natnum (error->line), make_natnum (error->column),
                  make_natnum (error->position)));
}

static void
json_release_object (void *object)
{
  json_decref (object);
}

static void
check_string_without_embedded_nulls (Lisp_Object object)
{
  CHECK_STRING (object);
  CHECK_TYPE (memchr (SDATA (object), '\0', SBYTES (object)) == NULL,
              Qstring_without_embedded_nulls_p, object);
}

static json_t *
json_check (json_t *object)
{
  if (object == NULL)
    json_out_of_memory ();
  return object;
}

/* This returns Lisp_Object so we can use unbind_to.  The return value
   is always nil.  */

static Lisp_Object
lisp_to_json (Lisp_Object lisp, json_t **json)
{
  if (NILP (lisp))
    {
      *json =json_check (json_null ());
      return Qnil;
    }
  else if (EQ (lisp, QCjson_false))
    {
      *json = json_check (json_false ());
      return Qnil;
    }
  else if (EQ (lisp, Qt))
    {
      *json = json_check (json_true ());
      return Qnil;
    }
  else if (INTEGERP (lisp))
    {
      CHECK_TYPE_RANGED_INTEGER (json_int_t, lisp);
      *json = json_check (json_integer (XINT (lisp)));
      return Qnil;
    }
  else if (FLOATP (lisp))
    {
      *json = json_check (json_real (XFLOAT_DATA (lisp)));
      return Qnil;
    }
  else if (STRINGP (lisp))
    {
      ptrdiff_t size = SBYTES (lisp);
      eassert (size >= 0);
      if (size > SIZE_MAX)
        xsignal1 (Qoverflow_error, build_pure_c_string ("string is too long"));
      *json = json_check (json_stringn (SSDATA (lisp), size));
      return Qnil;
    }
  else if (VECTORP (lisp))
    {
      if (++lisp_eval_depth > max_lisp_eval_depth)
        xsignal0 (Qjson_object_too_deep);
      ptrdiff_t size = ASIZE (lisp);
      eassert (size >= 0);
      if (size > SIZE_MAX)
        xsignal1 (Qoverflow_error, build_pure_c_string ("vector is too long"));
      *json = json_check (json_array ());
      ptrdiff_t count = SPECPDL_INDEX ();
      record_unwind_protect_ptr (json_release_object, json);
      for (ptrdiff_t i = 0; i < size; ++i)
        {
          json_t *element;
          lisp_to_json (AREF (lisp, i), &element);
          int status = json_array_append_new (*json, element);
          if (status == -1)
            json_out_of_memory ();
          eassert (status == 0);
        }
      eassert (json_array_size (*json) == size);
      clear_unwind_protect (count);
      --lisp_eval_depth;
      return unbind_to (count, Qnil);
    }
  else if (HASH_TABLE_P (lisp))
    {
      if (++lisp_eval_depth > max_lisp_eval_depth)
        xsignal0 (Qjson_object_too_deep);
      struct Lisp_Hash_Table *h = XHASH_TABLE (lisp);
      *json = json_check (json_object ());
      ptrdiff_t count = SPECPDL_INDEX ();
      record_unwind_protect_ptr (json_release_object, *json);
      for (ptrdiff_t i = 0; i < HASH_TABLE_SIZE (h); ++i)
        if (!NILP (HASH_HASH (h, i)))
          {
            Lisp_Object key = HASH_KEY (h, i);
            /* We can’t specify the length, so the string must be
               null-terminated.  */
            check_string_without_embedded_nulls (key);
            json_t *value;
            lisp_to_json (HASH_VALUE (h, i), &value);
            int status = json_object_set_new (*json, SSDATA (key), value);
            if (status == -1)
              json_out_of_memory ();
            eassert (status == 0);
          }
      clear_unwind_protect (count);
      --lisp_eval_depth;
      return unbind_to (count, Qnil);
    }
  wrong_type_argument (Qjson_value_p, lisp);
}

DEFUN ("json-serialize", Fjson_serialize, Sjson_serialize, 1, 1, NULL,
       doc: /* Return the JSON representation of OBJECT as a string.
OBJECT must be a vector or hashtable, and its elements can recursively
contain nil, t, `:json-false', numbers, strings, or other vectors and
hashtables.  nil, t, and `:json-false' will be converted to JSON null,
true, and false values, respectively.  Vectors will be converted to
JSON arrays, and hashtables to JSON objects.  Hashtable keys must be
strings without embedded null characters and must be unique within
each object.  */)
  (Lisp_Object object)
{
  ptrdiff_t count = SPECPDL_INDEX ();

  json_t *json;
  lisp_to_json (object, &json);
  record_unwind_protect_ptr (json_release_object, json);

  char *string = json_dumps (json, JSON_COMPACT);
  if (string == NULL)
    json_out_of_memory ();
  record_unwind_protect_ptr (free, string);

  return unbind_to (count, build_string (string));
}

struct json_buffer_and_size
{
  const char *buffer;
  size_t size;
};

static Lisp_Object
json_insert (Lisp_Object data)
{
  const struct json_buffer_and_size *buffer_and_size = XSAVE_POINTER (data, 0);
  if (FIXNUM_OVERFLOW_P (buffer_and_size->size))
    xsignal1 (Qoverflow_error, build_pure_c_string ("buffer too large"));
  Lisp_Object string
    = make_string (buffer_and_size->buffer, buffer_and_size->size);
  insert_from_string (string, 0, 0, SCHARS (string), SBYTES (string), false);
  return Qnil;
}

struct json_insert_data
{
  /* nil if json_insert succeeded, otherwise a cons
     (ERROR-SYMBOL . ERROR-DATA).  */
  Lisp_Object error;
};

static int
json_insert_callback (const char *buffer, size_t size, void *data)
{
  /* This function may not exit nonlocally.  */
  struct json_insert_data *d = data;
  struct json_buffer_and_size buffer_and_size
    = {.buffer = buffer, .size = size};
  d->error
    = internal_condition_case_1 (json_insert, make_save_ptr (&buffer_and_size),
                                 Qt, Fidentity);
  return 0;
}

DEFUN ("json-insert", Fjson_insert, Sjson_insert, 1, 1, NULL,
       doc: /* Insert the JSON representation of OBJECT before point.
This is the same as (insert (json-serialize OBJECT)), but potentially
faster.  See the function `json-serialize' for allowed values of
OBJECT.  */)
  (Lisp_Object object)
{
  ptrdiff_t count = SPECPDL_INDEX ();

  json_t *json;
  lisp_to_json (object, &json);
  record_unwind_protect_ptr (json_release_object, json);

  struct json_insert_data data;
  int status
    = json_dump_callback (json, json_insert_callback, &data, JSON_COMPACT);
  if (status == -1)
    json_out_of_memory ();
  eassert (status == 0);

  if (!NILP (data.error))
    xsignal (XCAR (data.error), XCDR (data.error));

  return unbind_to (count, Qnil);
}

static Lisp_Object
json_to_lisp (json_t *json)
{
  switch (json_typeof (json))
    {
    case JSON_NULL:
      return Qnil;
    case JSON_FALSE:
      return QCjson_false;
    case JSON_TRUE:
      return Qt;
    case JSON_INTEGER:
      {
        json_int_t value = json_integer_value (json);
        if (FIXNUM_OVERFLOW_P (value))
          xsignal1 (Qoverflow_error,
                    build_pure_c_string ("JSON integer is too large"));
        return make_number (value);
      }
    case JSON_REAL:
      return make_float (json_real_value (json));
    case JSON_STRING:
      {
        size_t size = json_string_length (json);
        if (FIXNUM_OVERFLOW_P (size))
          xsignal1 (Qoverflow_error,
                    build_pure_c_string ("JSON string is too long"));
        return make_string (json_string_value (json), size);
      }
    case JSON_ARRAY:
      {
        if (++lisp_eval_depth > max_lisp_eval_depth)
          xsignal0 (Qjson_object_too_deep);
        size_t size = json_array_size (json);
        if (FIXNUM_OVERFLOW_P (size))
          xsignal1 (Qoverflow_error,
                    build_pure_c_string ("JSON array is too long"));
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
          xsignal1 (Qoverflow_error,
                    build_pure_c_string ("JSON object has too many elements"));
        Lisp_Object result = CALLN (Fmake_hash_table, QCtest, Qequal,
                                    QCsize, make_natnum (size));
        struct Lisp_Hash_Table *h = XHASH_TABLE (result);
        const char *key_str;
        json_t *value;
        json_object_foreach (json, key_str, value)
          {
            Lisp_Object key = build_string (key_str);
            EMACS_UINT hash;
            ptrdiff_t i = hash_lookup (h, key, &hash);
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
will be nil, t, `:json-false', numbers, strings, or further vectors
and hashtables.  If there are duplicate keys in an object, all but the
last one are ignored.  If STRING doesn't contain a valid JSON object,
an error of type `json-parse-error' is signaled.  */)
  (Lisp_Object string)
{
  ptrdiff_t count = SPECPDL_INDEX ();
  check_string_without_embedded_nulls (string);

  json_error_t error;
  json_t *object = json_loads (SSDATA (string), 0, &error);
  if (object == NULL)
    json_parse_error (&error);

  /* Avoid leaking the object in case of further errors.  */
  if (object != NULL)
    record_unwind_protect_ptr (json_release_object, object);

  return unbind_to (count, json_to_lisp (object));
}

struct json_read_buffer_data
{
  ptrdiff_t point;
};

static size_t
json_read_buffer_callback (void *buffer, size_t buflen, void *data)
{
  struct json_read_buffer_data *d = data;

  /* First, parse from point to the gap or the end of the accessible
     portion, whatever is closer.  */
  ptrdiff_t point = d->point;
  ptrdiff_t end;
  {
    bool overflow = INT_ADD_WRAPV (BUFFER_CEILING_OF (point), 1, &end);
    eassert (!overflow);
  }
  size_t count;
  {
    bool overflow = INT_SUBTRACT_WRAPV (end, point, &count);
    eassert (!overflow);
  }
  if (buflen < count)
    count = buflen;
  memcpy (buffer, BYTE_POS_ADDR (point), count);
  {
    bool overflow = INT_ADD_WRAPV (d->point, count, &d->point);
    eassert (!overflow);
  }
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

  {
    /* Adjust point by how much we just read.  Do this here because
       tokener->char_offset becomes incorrect below.  */
    bool overflow = INT_ADD_WRAPV (point, error.position, &point);
    eassert (!overflow);
    eassert (point <= ZV_BYTE);
    SET_PT_BOTH (BYTE_TO_CHAR (point), point);
  }

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
  DEFSYM (QCjson_false, ":json-false");

  DEFSYM (Qstring_without_embedded_nulls_p, "string-without-embedded-nulls-p");
  DEFSYM (Qjson_value_p, "json-value-p");

  DEFSYM (Qjson_error, "json-error");
  DEFSYM (Qjson_out_of_memory, "json-out-of-memory");
  DEFSYM (Qjson_parse_error, "json-parse-error");
  DEFSYM (Qjson_object_too_deep, "json-object-too-deep");
  define_error (Qjson_error, "generic JSON error", Qerror);
  define_error (Qjson_out_of_memory, "no free memory for creating JSON object",
                Qjson_error);
  define_error (Qjson_parse_error, "could not parse JSON stream",
                Qjson_error);
  define_error (Qjson_object_too_deep, "object cyclic or too deep",
                Qjson_error);

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
