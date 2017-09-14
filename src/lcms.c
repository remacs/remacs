/* Interface to Little CMS
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

#ifdef HAVE_LCMS2

#include <lcms2.h>
#include <math.h>

#include "lisp.h"

#ifdef WINDOWSNT
# include <windows.h>
# include "w32.h"

DEF_DLL_FN (cmsFloat64Number, cmsCIE2000DeltaE,
	    (const cmsCIELab* Lab1, const cmsCIELab* Lab2, cmsFloat64Number Kl,
	     cmsFloat64Number Kc, cmsFloat64Number Kh));
DEF_DLL_FN (cmsHANDLE, cmsCIECAM02Init,
	    (cmsContext ContextID, const cmsViewingConditions* pVC));
DEF_DLL_FN (void, cmsCIECAM02Forward,
	    (cmsHANDLE hModel, const cmsCIEXYZ* pIn, cmsJCh* pOut));
DEF_DLL_FN (void, cmsCIECAM02Done, (cmsHANDLE hModel));

static bool lcms_initialized;

static bool
init_lcms_functions (void)
{
  HMODULE library = w32_delayed_load (Qlcms2);

  if (!library)
    return false;

  LOAD_DLL_FN (library, cmsCIE2000DeltaE);
  LOAD_DLL_FN (library, cmsCIECAM02Init);
  LOAD_DLL_FN (library, cmsCIECAM02Forward);
  LOAD_DLL_FN (library, cmsCIECAM02Done);
  return true;
}

# undef cmsCIE2000DeltaE
# undef cmsCIECAM02Init
# undef cmsCIECAM02Forward
# undef cmsCIECAM02Done

# define cmsCIE2000DeltaE   fn_cmsCIE2000DeltaE
# define cmsCIECAM02Init    fn_cmsCIECAM02Init
# define cmsCIECAM02Forward fn_cmsCIECAM02Forward
# define cmsCIECAM02Done    fn_cmsCIECAM02Done

#endif	/* WINDOWSNT */

static bool
parse_lab_list (Lisp_Object lab_list, cmsCIELab *color)
{
#define PARSE_LAB_LIST_FIELD(field)					\
  if (CONSP (lab_list) && NUMBERP (XCAR (lab_list)))			\
    {									\
      color->field = XFLOATINT (XCAR (lab_list));			\
      lab_list = XCDR (lab_list);					\
    }									\
  else									\
    return false;

  PARSE_LAB_LIST_FIELD (L);
  PARSE_LAB_LIST_FIELD (a);
  PARSE_LAB_LIST_FIELD (b);

  return true;
}

/* http://www.ece.rochester.edu/~gsharma/ciede2000/ciede2000noteCRNA.pdf> */

DEFUN ("lcms-cie-de2000", Flcms_cie_de2000, Slcms_cie_de2000, 2, 5, 0,
       doc: /* Compute CIEDE2000 metric distance between COLOR1 and COLOR2.
Each color is a list of L*a*b* coordinates, where the L* channel ranges from
0 to 100, and the a* and b* channels range from -128 to 128.
Optional arguments KL, KC, KH are weighting parameters for lightness,
chroma, and hue, respectively. The parameters each default to 1. */)
  (Lisp_Object color1, Lisp_Object color2,
   Lisp_Object kL, Lisp_Object kC, Lisp_Object kH)
{
  cmsCIELab Lab1, Lab2;
  cmsFloat64Number Kl, Kc, Kh;

#ifdef WINDOWSNT
  if (!lcms_initialized)
    lcms_initialized = init_lcms_functions ();
  if (!lcms_initialized)
    {
      message1 ("lcms2 library not found");
      return Qnil;
    }
#endif

  if (!(CONSP (color1) && parse_lab_list (color1, &Lab1)))
    signal_error ("Invalid color", color1);
  if (!(CONSP (color2) && parse_lab_list (color2, &Lab2)))
    signal_error ("Invalid color", color1);
  if (NILP (kL))
    Kl = 1.0f;
  else if (!(NUMBERP (kL) && (Kl = XFLOATINT(kL))))
    wrong_type_argument(Qnumberp, kL);
  if (NILP (kC))
    Kc = 1.0f;
  else if (!(NUMBERP (kC) && (Kc = XFLOATINT(kC))))
    wrong_type_argument(Qnumberp, kC);
  if (NILP (kL))
    Kh = 1.0f;
  else if (!(NUMBERP (kH) && (Kh = XFLOATINT(kH))))
    wrong_type_argument(Qnumberp, kH);

  return make_float (cmsCIE2000DeltaE (&Lab1, &Lab2, Kl, Kc, Kh));
}

/* FIXME: code duplication */

static bool
parse_xyz_list (Lisp_Object xyz_list, cmsCIEXYZ *color)
{
#define PARSE_XYZ_LIST_FIELD(field)					\
  if (CONSP (xyz_list) && NUMBERP (XCAR (xyz_list)))			\
    {									\
      color->field = 100.0 * XFLOATINT (XCAR (xyz_list));		\
      xyz_list = XCDR (xyz_list);					\
    }									\
  else									\
    return false;

  PARSE_XYZ_LIST_FIELD (X);
  PARSE_XYZ_LIST_FIELD (Y);
  PARSE_XYZ_LIST_FIELD (Z);

  return true;
}

DEFUN ("lcms-cam02-ucs", Flcms_cam02_ucs, Slcms_cam02_ucs, 2, 3, 0,
       doc: /* Compute CAM02-UCS metric distance between COLOR1 and COLOR2.
Each color is a list of XYZ coordinates, with Y scaled to unity.
Optional argument is the XYZ white point, which defaults to illuminant D65. */)
  (Lisp_Object color1, Lisp_Object color2, Lisp_Object whitepoint)
{
  cmsViewingConditions vc;
  cmsJCh jch1, jch2;
  cmsHANDLE h1, h2;
  cmsCIEXYZ xyz1, xyz2, xyzw;
  double Jp1, ap1, bp1, Jp2, ap2, bp2;
  double Mp1, Mp2, FL, k, k4;

#ifdef WINDOWSNT
  if (!lcms_initialized)
    lcms_initialized = init_lcms_functions ();
  if (!lcms_initialized)
    {
      message1 ("lcms2 library not found");
      return Qnil;
    }
#endif

  if (!(CONSP (color1) && parse_xyz_list (color1, &xyz1)))
    signal_error ("Invalid color", color1);
  if (!(CONSP (color2) && parse_xyz_list (color2, &xyz2)))
    signal_error ("Invalid color", color1);
  if (NILP (whitepoint))
    {
      xyzw.X = 95.047;
      xyzw.Y = 100.0;
      xyzw.Z = 108.883;
    }
  else if (!(CONSP (whitepoint) && parse_xyz_list(whitepoint, &xyzw)))
    signal_error("Invalid white point", whitepoint);

  vc.whitePoint.X = xyzw.X;
  vc.whitePoint.Y = xyzw.Y;
  vc.whitePoint.Z = xyzw.Z;
  vc.Yb = 20;
  vc.La = 100;
  vc.surround = AVG_SURROUND;
  vc.D_value = 1.0;

  h1 = cmsCIECAM02Init (0, &vc);
  h2 = cmsCIECAM02Init (0, &vc);
  cmsCIECAM02Forward (h1, &xyz1, &jch1);
  cmsCIECAM02Forward (h2, &xyz2, &jch2);
  cmsCIECAM02Done (h1);
  cmsCIECAM02Done (h2);

  /* Now have colors in JCh, need to calculate J'a'b'

     M = C * F_L^0.25
     J' = 1.7 J / (1 + 0.007 J)
     M' = 43.86 ln(1 + 0.0228 M)
     a' = M' cos(h)
     b' = M' sin(h)

     where

     F_L = 0.2 k^4 (5 L_A) + 0.1 (1 - k^4)^2 (5 L_A)^(1/3),
     k = 1/(5 L_A + 1)
  */
  k = 1.0 / (1.0 + (5.0 * vc.La));
  k4 = k * k * k * k;
  FL = vc.La * k4 + 0.1 * (1 - k4) * (1 - k4) * cbrt (5.0 * vc.La);
  Mp1 = 43.86 * log (1.0 + 0.0228 * (jch1.C * sqrt (sqrt (FL))));
  Mp2 = 43.86 * log (1.0 + 0.0228 * (jch2.C * sqrt (sqrt (FL))));
  Jp1 = 1.7 * jch1.J / (1.0 + (0.007 * jch1.J));
  Jp2 = 1.7 * jch2.J / (1.0 + (0.007 * jch2.J));
  ap1 = Mp1 * cos (jch1.h);
  ap2 = Mp2 * cos (jch2.h);
  bp1 = Mp1 * sin (jch1.h);
  bp2 = Mp2 * sin (jch2.h);

  return make_float (sqrt ((Jp2 - Jp1) * (Jp2 - Jp1) +
                           (ap2 - ap1) * (ap2 - ap1) +
                           (bp2 - bp1) * (bp2 - bp1)));
}

DEFUN ("lcms2-available-p", Flcms2_available_p, Slcms2_available_p, 0, 0, 0,
       doc: /* Return t if lcms2 color calculations are available in this instance of Emacs.  */)
     (void)
{
#ifdef WINDOWSNT
  Lisp_Object found = Fassq (Qlcms2, Vlibrary_cache);
  if (CONSP (found))
    return XCDR (found);
  else
    {
      Lisp_Object status;
      lcms_initialized = init_lcms_functions ();
      status = lcms_initialized ? Qt : Qnil;
      Vlibrary_cache = Fcons (Fcons (Qlcms2, status), Vlibrary_cache);
      return status;
    }
#else  /* !WINDOWSNT */
  return Qt;
#endif
}


/* Initialization */
void
syms_of_lcms2 (void)
{
  defsubr (&Slcms_cie_de2000);
  defsubr (&Slcms_cam02_ucs);
  defsubr (&Slcms2_available_p);

  Fprovide (intern_c_string ("lcms2"), Qnil);
}

#endif /* HAVE_LCMS2 */
