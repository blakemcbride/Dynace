# Microsoft Developer Studio Generated NMAKE File, Format Version 40001
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) External Target" 0x0106

!IF "$(CFG)" == ""
CFG=main - Win32 Debug
!MESSAGE No configuration specified.  Defaulting to main - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "main - Win32 Release" && "$(CFG)" != "main - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "main.mak" CFG="main - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "main - Win32 Release" (based on "Win32 (x86) External Target")
!MESSAGE "main - Win32 Debug" (based on "Win32 (x86) External Target")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 
################################################################################
# Begin Project
# PROP Target_Last_Scanned "main - Win32 Debug"

!IF  "$(CFG)" == "main - Win32 Release"

# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir ""
# PROP BASE Intermediate_Dir ""
# PROP BASE Target_Dir ""
# PROP BASE Cmd_Line "dmake -f m32.dm"
# PROP BASE Rebuild_Opt ""
# PROP BASE Target_File "main.exe"
# PROP BASE Bsc_Name "main.bsc"
# PROP Use_Debug_Libraries 0
# PROP Output_Dir ""
# PROP Intermediate_Dir ""
# PROP Target_Dir ""
# PROP Cmd_Line "dmake -f m32.dm"
# PROP Rebuild_Opt ""
# PROP Target_File "main.exe"
# PROP Bsc_Name "main.bsc"
OUTDIR=.
INTDIR=.

ALL : 

CLEAN : 
	-@erase 

!ELSEIF  "$(CFG)" == "main - Win32 Debug"

# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir ""
# PROP BASE Intermediate_Dir ""
# PROP BASE Target_Dir ""
# PROP BASE Cmd_Line "dmake -f m32.dm DEBUG=1"
# PROP BASE Rebuild_Opt ""
# PROP BASE Target_File "main.exe"
# PROP BASE Bsc_Name "main.bsc"
# PROP Use_Debug_Libraries 1
# PROP Output_Dir ""
# PROP Intermediate_Dir ""
# PROP Target_Dir ""
# PROP Cmd_Line "dmake -f m32.dm DEBUG=1"
# PROP Rebuild_Opt ""
# PROP Target_File "main.exe"
# PROP Bsc_Name "main.bsc"
OUTDIR=.
INTDIR=.

ALL : 

CLEAN : 
	-@erase 

!ENDIF 

################################################################################
# Begin Target

# Name "main - Win32 Release"
# Name "main - Win32 Debug"

!IF  "$(CFG)" == "main - Win32 Release"

"$(OUTDIR)\main.exe" : 
   dmake -f m32.dm

!ELSEIF  "$(CFG)" == "main - Win32 Debug"

"$(OUTDIR)\main.exe" : 
   dmake -f m32.dm DEBUG=1

!ENDIF 

################################################################################
# Begin Source File

SOURCE=.\m32.dm

!IF  "$(CFG)" == "main - Win32 Release"

!ELSEIF  "$(CFG)" == "main - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\main.c

!IF  "$(CFG)" == "main - Win32 Release"

!ELSEIF  "$(CFG)" == "main - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\class1.d

!IF  "$(CFG)" == "main - Win32 Release"

!ELSEIF  "$(CFG)" == "main - Win32 Debug"

!ENDIF 

# End Source File
# End Target
# End Project
################################################################################
