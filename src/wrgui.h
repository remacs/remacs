/* Definitions and headers for webrender GUI backend.
   Copyright (C) 1995, 2001-2017 Free Software Foundation, Inc.

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

#ifndef EMACS_WRGUI_H
#define EMACS_WRGUI_H

/* Emulate X GC's by keeping color and font info in a structure.  */
typedef struct _XGCValues
{
  unsigned long foreground;
  unsigned long background;
  struct font *font;
} XGCValues;

#define GCForeground 0x01
#define GCBackground 0x02
#define GCFont 0x03
#define GCGraphicsExposures 0

typedef void* Pixmap;

typedef char * XrmDatabase;

typedef XGCValues * GC;
typedef int Color;
typedef int Window;
typedef int Display;  /* HDC so it doesn't conflict with xpm lib.  */
typedef int Cursor;
typedef int RGB_PIXEL_COLOR;

#define No_Cursor (0)
#define XChar2b wchar_t

/* Windows equivalent of XImage.  */
typedef struct _WRImage
{
  unsigned char * data;
  int info;
  /* Optional RGBQUAD array for palette follows (see BITMAPINFO docs).  */
} WRImage;


typedef struct {
    int x, y;
    unsigned width, height;
} XRectangle;

#define NativeRectangle int

#define CONVERT_TO_XRECT(xr,nr)	 {}

#define CONVERT_FROM_XRECT(xr,nr)      {}	

#define STORE_NATIVE_RECT(nr,x,y,width,height)	{}

#define STORE_XCHAR2B(chp, b1, b2) \
  (*(chp) = ((XChar2b)((((b1) & 0x00ff) << 8) | ((b2) & 0x00ff))))

#define XCHAR2B_BYTE1(chp) \
  ((*(chp) & 0xff00) >> 8)

#define XCHAR2B_BYTE2(chp) \
  (*(chp) & 0x00ff)

/* Bit Gravity */

#define ForgetGravity		0
#define NorthWestGravity	1
#define NorthGravity		2
#define NorthEastGravity	3
#define WestGravity		4
#define CenterGravity		5
#define EastGravity		6
#define SouthWestGravity	7
#define SouthGravity		8
#define SouthEastGravity	9
#define StaticGravity		10

#define NoValue		0x0000
#define XValue  	0x0001
#define YValue		0x0002
#define WidthValue  	0x0004
#define HeightValue  	0x0008
#define AllValues 	0x000F
#define XNegative 	0x0010
#define YNegative 	0x0020

#define USPosition	(1L << 0) /* user specified x, y */
#define USSize		(1L << 1) /* user specified width, height */

#define PPosition	(1L << 2) /* program specified position */
#define PSize		(1L << 3) /* program specified size */
#define PMinSize	(1L << 4) /* program specified minimum size */
#define PMaxSize	(1L << 5) /* program specified maximum size */
#define PResizeInc	(1L << 6) /* program specified resize increments */
#define PAspect		(1L << 7) /* program specified min and max aspect ratios */
#define PBaseSize	(1L << 8) /* program specified base for incrementing */
#define PWinGravity	(1L << 9) /* program specified window gravity */


#endif /* EMACS_WRGUI_H */
