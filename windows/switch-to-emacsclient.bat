@echo off
set default_binpath="C:\Emacs\bin"
if exist %default_binpath% (
   set binpath=%default_binpath%
) else (
   set binpath=%EMACS%\bin
)

:: If no arg is given set filename to touch
if "%~1"=="" (
  set filename="~\.emacs.d\touch"
) else (
  set filename=%~1
)

:: Run Emacsclient
rem or "runemacs.exe -xrm Emacs.FontBackend:gdi" for rendering & scrolling perfomance
"%binpath%\emacsclientw.exe" --no-wait --alternate-editor="%binpath%\runemacs.exe" %filename%
exit
