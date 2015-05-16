### lisp.mk --- src/Makefile fragment for GNU Emacs

## Copyright (C) 1985, 1987-1988, 1993-1995, 1999-2015 Free Software
## Foundation, Inc.

## This file is part of GNU Emacs.

## GNU Emacs is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.

## GNU Emacs is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

### Commentary:

## This is the list of all Lisp files that might be loaded into the
## dumped Emacs.  Some of them are not loaded on all platforms, but
## the DOC file on every platform uses them (because the DOC file is
## supposed to be platform-independent).
## It is arranged like this because it is easier to generate it
## semi-mechanically from loadup.el this way.
## Eg something like:
##   sed -e 's/"[ )].*//' -n -e '/(load "/ s/.*load "//p' loadup.el | \
##   grep -vE 'site-|ldefs-boot'
## minus any duplicates.
## Note that you can generally just add a ".elc" extension to every file
## that does not have an explicit .el extension, but beware of any
## no-byte-compile ones.

## Note that this list should not include lisp files which might not
## be present, like site-load.el and site-init.el; this makefile
## expects them all to be either present or buildable.

## Place loaddefs.el first, so it gets generated first, since it is on
## the critical path (relevant in parallel compilations).

### Code:

shortlisp = \
	loaddefs.el \
	loadup.el \
	emacs-lisp/byte-run.elc \
	emacs-lisp/backquote.elc \
	subr.elc \
	version.elc \
	widget.elc \
	custom.elc \
	emacs-lisp/map-ynp.elc \
	cus-start.elc \
	international/mule.elc \
	international/mule-conf.elc \
	env.elc \
	format.elc \
	bindings.elc \
	files.elc \
	emacs-lisp/macroexp.elc \
	cus-face.elc \
	faces.elc \
	button.elc \
	startup.elc \
	emacs-lisp/cl-preloaded.elc \
	emacs-lisp/nadvice.elc \
	minibuffer.elc \
	abbrev.elc \
	simple.elc \
	help.elc \
	jka-cmpr-hook.elc \
	epa-hook.elc \
	international/mule-cmds.elc \
	case-table.elc \
	international/characters.elc \
	composite.elc \
	international/charprop.el \
	language/chinese.elc \
	language/cyrillic.elc \
	language/indian.elc \
	language/sinhala.elc \
	language/english.elc \
	language/ethiopic.elc \
	language/european.elc \
	language/czech.elc \
	language/slovak.elc \
	language/romanian.elc \
	language/greek.elc \
	language/hebrew.elc \
	international/cp51932.el \
	international/eucjp-ms.el \
	language/japanese.elc \
	language/korean.elc \
	language/lao.elc \
	language/tai-viet.elc \
	language/thai.elc \
	language/tibetan.elc \
	language/vietnamese.elc \
	language/misc-lang.elc \
	language/utf-8-lang.elc \
	language/georgian.elc \
	language/khmer.elc \
	language/burmese.elc \
	language/cham.elc \
	indent.elc \
	window.elc \
	emacs-lisp/cl-generic.elc \
	frame.elc \
	term/tty-colors.elc \
	font-core.elc \
	facemenu.elc \
	emacs-lisp/syntax.elc \
	font-lock.elc \
	jit-lock.elc \
	mouse.elc \
	scroll-bar.elc \
	select.elc \
	emacs-lisp/timer.elc \
	isearch.elc \
	rfn-eshadow.elc \
	menu-bar.elc \
	emacs-lisp/lisp.elc \
	textmodes/page.elc \
	register.elc \
	textmodes/paragraphs.elc \
	progmodes/prog-mode.elc \
	emacs-lisp/lisp-mode.elc \
	progmodes/elisp-mode.elc \
	textmodes/text-mode.elc \
	textmodes/fill.elc \
	newcomment.elc \
	replace.elc \
	emacs-lisp/tabulated-list.elc \
	buff-menu.elc \
	fringe.elc \
	emacs-lisp/regexp-opt.elc \
	image.elc \
	international/fontset.elc \
	dnd.elc \
	tool-bar.elc \
	dynamic-setting.elc \
	x-dnd.elc \
	term/common-win.elc \
	term/x-win.elc \
	w32-vars.elc \
	term/w32-win.elc \
	ls-lisp.elc \
	disp-table.elc \
	dos-w32.elc \
	w32-fns.elc \
	dos-fns.elc \
	dos-vars.elc \
	term/pc-win.elc \
	term/internal.elc \
	term/ns-win.elc \
	mwheel.elc \
	emacs-lisp/float-sup.elc \
	vc/vc-hooks.elc \
	vc/ediff-hook.elc \
	electric.elc \
	emacs-lisp/eldoc.elc \
	uniquify.elc \
	tooltip.elc

### lisp.mk ends here
