!include MUI2.nsh


Outfile "Emacs-${ARCH}-${OUT_VERSION}-installer.exe"



InstallDir "$DESKTOP\Emacs-${EMACS_VERSION}"
SetCompressor /solid lzma


Var StartMenuFolder


!define MUI_WELCOMEPAGE_TITLE "Emacs"
!define MUI_WELCOMEPAGE_TITLE_3LINES
!define MUI_WELCOMEPAGE_TEXT "Welcome to Emacs -- the editor of a lifetime."

!define MUI_WELCOMEFINISHPAGE_BITMAP "${ARCH}\share\emacs\${EMACS_VERSION}\etc\images\splash.bmp"

!insertmacro MUI_PAGE_WELCOME


!define MUI_LICENSEPAGE_TEXT_TOP "The GNU General Public License"
!insertmacro MUI_PAGE_LICENSE "${ARCH}\share\emacs\${EMACS_VERSION}\lisp\COPYING"

!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES

!insertmacro MUI_PAGE_STARTMENU Application $StartMenuFolder

!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES

!insertmacro MUI_LANGUAGE "English"
Name Emacs-${EMACS_VERSION}

Section

  SetOutPath $INSTDIR

  File /r ${ARCH}
  # define uninstaller name
  WriteUninstaller $INSTDIR\Uninstall.exe

  !insertmacro MUI_STARTMENU_WRITE_BEGIN Application
  ;Create shortcuts
  CreateDirectory "$SMPROGRAMS\$StartMenuFolder"
  CreateShortcut "$SMPROGRAMS\$StartMenuFolder\Uninstall.lnk" "$INSTDIR\Uninstall.exe"

  !insertmacro MUI_STARTMENU_WRITE_END
  CreateShortCut "$SMPROGRAMS\$StartMenuFolder\Emacs.lnk" "$INSTDIR\${ARCH}\bin\runemacs.exe"
SectionEnd


# create a section to define what the uninstaller does.
# the section will always be named "Uninstall"
Section "Uninstall"

  # Always delete uninstaller first
  Delete "$INSTDIR\Uninstall.exe"

  # now delete installed directory
  RMDir /r "$INSTDIR\${ARCH}"
  RMDir "$INSTDIR"

  !insertmacro MUI_STARTMENU_GETFOLDER Application $StartMenuFolder

  Delete "$SMPROGRAMS\$StartMenuFolder\Uninstall.lnk"
  RMDir "$SMPROGRAMS\$StartMenuFolder"
SectionEnd
