typedef struct
{
  /* The current charset for which the following tables are setup.  */
  struct charset *current;

  /* 1 iff the following table is used for encoder.  */
  short for_encoder;

  /* When the following table is used for encoding, minimum and
     maximum character of the current charset.  */
  int min_char, max_char;

  /* A Unicode character corresponding to the code index 0 (i.e. the
     minimum code-point) of the current charset, or -1 if the code
     index 0 is not a Unicode character.  This is checked when
     table.encoder[CHAR] is zero.  */
  int zero_index_char;

  union {
    /* Table mapping code-indices (not code-points) of the current
       charset to Unicode characters.  If decoder[CHAR] is -1, CHAR
       doesn't belong to the current charset.  */
    int decoder[0x10000];
    /* Table mapping Unicode characters to code-indices of the current
       charset.  The first 0x10000 elements are for BMP (0..0xFFFF),
       and the last 0x10000 are for SMP (0x10000..0x1FFFF) or SIP
       (0x20000..0x2FFFF).  Note that there is no charset map that
       uses both SMP and SIP.  */
    unsigned short encoder[0x20000];
  } table;
} TempCharsetWork;

extern TempCharsetWork *temp_charset_work;

#define SET_TEMP_CHARSET_WORK_ENCODER(C, CODE)			\
  do {								\
    if ((CODE) == 0)						\
      temp_charset_work->zero_index_char = (C);			\
    else if ((C) < 0x20000)					\
      temp_charset_work->table.encoder[(C)] = (CODE);		\
    else							\
      temp_charset_work->table.encoder[(C) - 0x10000] = (CODE);	\
  } while (0)

#define GET_TEMP_CHARSET_WORK_ENCODER(C)				  \
  ((C) == temp_charset_work->zero_index_char ? 0			  \
   : (C) < 0x20000 ? (temp_charset_work->table.encoder[(C)]		  \
		      ? (int) temp_charset_work->table.encoder[(C)] : -1) \
   : temp_charset_work->table.encoder[(C) - 0x10000]			  \
   ? temp_charset_work->table.encoder[(C) - 0x10000] : -1)

#define SET_TEMP_CHARSET_WORK_DECODER(C, CODE)	\
  (temp_charset_work->table.decoder[(CODE)] = (C))

#define GET_TEMP_CHARSET_WORK_DECODER(CODE)	\
  (temp_charset_work->table.decoder[(CODE)])
