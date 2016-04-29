$ SET NOVERIFY
$!
$! ES40 Emulator
$! Copyright (C) 2007-2008 by the ES40 Emulator Project
$!
$! This file was created by make_vms.sh. Please refer to that file
$! for more information.
$!
$ SAY = "WRITE SYS$OUTPUT"
$!
$! DETERMINE ES40 SRC ROOT PATH IN UNIX-STYLE SYNTAX
$!
$ DFLT = F$STRING("/" + F$ENVIRONMENT("DEFAULT"))
$ DLEN = F$LENGTH("''DFLT'")
$!
$ loop_dot:
$   DD = F$LOCATE(".",DFLT)
$   IF DD .EQ. DLEN
$   THEN
$     GOTO loop_dot_end
$   ENDIF
$   DFLT[DD,1]:="/"
$ GOTO loop_dot
$ loop_dot_end:
$!
$ DD = F$LOCATE(":[",DFLT)
$ IF DD .NE. DLEN
$ THEN
$   DFLT[DD,2]:="/"
$ ENDIF
$!
$ DD = F$LOCATE("]",DFLT)
$ IF DD .NE. DLEN
$ THEN
$   DFLT[DD,1]:=""
$ ENDIF
$!
$ DD = F$LOCATE("$",DFLT)
$ IF DD .NE. DLEN
$ THEN
$   DFLT=F$STRING(F$EXTRACT(0,DD,DFLT) + "\$" + F$EXTRACT(DD+1,DLEN-DD,DFLT))
$ ENDIF
$!
$ ES40_ROOT = F$EDIT(DFLT,"COLLAPSE")
$!
$! Determine if X11 support is available...
$!
$ CREATE X11TEST.CPP
$ DECK
#include <X11/Xlib.h>

void x() { XOpenDisplay(NULL); }
$ EOD
$ SET NOON
$ CXX X11TEST.CPP /OBJECT=X11TEST.OBJ
$ IF $STATUS
$ THEN
$   SAY "Have found X11 support"
$   X11_DEF=",HAVE_X11"
$   X11_LIB=",SYS$LIBRARY:DECWINDOWS/LIB"
$ ELSE
$   SAY "Have not found X11 support"
$   X11_DEF=""
$   X11_LIB=""
$ ENDIF
$ DELETE X11TEST.CPP;
$ DELETE X11TEST.OBJ;
$ SET ON
$!
$!
$! Compile sources for es40
$!
$! Compile with the following defines: ES40,__USE_STD_IOSTREAM
$!
$ SAY "Compiling es40..."
$!
$! Check if es40_AliM1543C.obj is up-to-date...
$!
$ SRCTIME = F$CVTIME(F$FILE_ATTRIBUTES("AliM1543C.cpp","RDT"),"COMPARISON")
$ OBJFILE = F$SEARCH("es40_AliM1543C.obj")
$ IF OBJFILE .NES. ""
$ THEN
$   OBJTIME = F$CVTIME(F$FILE_ATTRIBUTES("es40_AliM1543C.obj","RDT"),"COMPARISON")
$ ELSE
$   OBJTIME = F$CVTIME("01-JAN-1970 00:00:00.00","COMPARISON")
$ ENDIF
$!
$! Compile AliM1543C.cpp to es40_AliM1543C.obj
$ IF SRCTIME .GTS. OBJTIME
$ THEN
$   CXX AliM1543C.cpp -
         /DEFINE=(ES40,__USE_STD_IOSTREAM'X11_DEF') -
         /INCLUDE=("''ES40_ROOT'/GUI","''ES40_ROOT'/BASE'") -
         /STANDARD=GNU -
         /ARCHITECTURE=HOST -
         /OPTIMIZE=(LEVEL=4,INLINE=SPEED,TUNE=HOST) -
         /OBJECT=es40_AliM1543C.obj
$ ENDIF
$!
$! Link es40
$!
$ SAY "Linking es40..."
$!
$ CXXLINK es40_AliM1543C.obj'X11_LIB' -
           /EXECUTABLE=es40.exe
$!
$! Compile sources for es40_idb
$!
$! Compile with the following defines: ES40,__USE_STD_IOSTREAM,IDB
$!
$ SAY "Compiling es40_idb..."
$!
$! Check if es40_idb_AliM1543C.obj is up-to-date...
$!
$ SRCTIME = F$CVTIME(F$FILE_ATTRIBUTES("AliM1543C.cpp","RDT"),"COMPARISON")
$ OBJFILE = F$SEARCH("es40_idb_AliM1543C.obj")
$ IF OBJFILE .NES. ""
$ THEN
$   OBJTIME = F$CVTIME(F$FILE_ATTRIBUTES("es40_idb_AliM1543C.obj","RDT"),"COMPARISON")
$ ELSE
$   OBJTIME = F$CVTIME("01-JAN-1970 00:00:00.00","COMPARISON")
$ ENDIF
$!
$! Compile AliM1543C.cpp to es40_idb_AliM1543C.obj
$ IF SRCTIME .GTS. OBJTIME
$ THEN
$   CXX AliM1543C.cpp -
         /DEFINE=(ES40,__USE_STD_IOSTREAM,IDB'X11_DEF') -
         /INCLUDE=("''ES40_ROOT'/GUI","''ES40_ROOT'/BASE'") -
         /STANDARD=GNU -
         /ARCHITECTURE=HOST -
         /OPTIMIZE=(LEVEL=4,INLINE=SPEED,TUNE=HOST) -
         /OBJECT=es40_idb_AliM1543C.obj
$ ENDIF
$!
$! Link es40_idb
$!
$ SAY "Linking es40_idb..."
$!
$ CXXLINK es40_idb_AliM1543C.obj'X11_LIB' -
           /EXECUTABLE=es40_idb.exe
$!
$! Compile sources for es40_lss
$!
$! Compile with the following defines: ES40,__USE_STD_IOSTREAM,IDB,LSS
$!
$ SAY "Compiling es40_lss..."
$!
$! Check if es40_lss_AliM1543C.obj is up-to-date...
$!
$ SRCTIME = F$CVTIME(F$FILE_ATTRIBUTES("AliM1543C.cpp","RDT"),"COMPARISON")
$ OBJFILE = F$SEARCH("es40_lss_AliM1543C.obj")
$ IF OBJFILE .NES. ""
$ THEN
$   OBJTIME = F$CVTIME(F$FILE_ATTRIBUTES("es40_lss_AliM1543C.obj","RDT"),"COMPARISON")
$ ELSE
$   OBJTIME = F$CVTIME("01-JAN-1970 00:00:00.00","COMPARISON")
$ ENDIF
$!
$! Compile AliM1543C.cpp to es40_lss_AliM1543C.obj
$ IF SRCTIME .GTS. OBJTIME
$ THEN
$   CXX AliM1543C.cpp -
         /DEFINE=(ES40,__USE_STD_IOSTREAM,IDB,LSS'X11_DEF') -
         /INCLUDE=("''ES40_ROOT'/GUI","''ES40_ROOT'/BASE'") -
         /STANDARD=GNU -
         /ARCHITECTURE=HOST -
         /OPTIMIZE=(LEVEL=4,INLINE=SPEED,TUNE=HOST) -
         /OBJECT=es40_lss_AliM1543C.obj
$ ENDIF
$!
$! Link es40_lss
$!
$ SAY "Linking es40_lss..."
$!
$ CXXLINK es40_lss_AliM1543C.obj'X11_LIB' -
           /EXECUTABLE=es40_lss.exe
$!
$! Compile sources for es40_lsm
$!
$! Compile with the following defines: ES40,__USE_STD_IOSTREAM,IDB,LSM
$!
$ SAY "Compiling es40_lsm..."
$!
$! Check if es40_lsm_AliM1543C.obj is up-to-date...
$!
$ SRCTIME = F$CVTIME(F$FILE_ATTRIBUTES("AliM1543C.cpp","RDT"),"COMPARISON")
$ OBJFILE = F$SEARCH("es40_lsm_AliM1543C.obj")
$ IF OBJFILE .NES. ""
$ THEN
$   OBJTIME = F$CVTIME(F$FILE_ATTRIBUTES("es40_lsm_AliM1543C.obj","RDT"),"COMPARISON")
$ ELSE
$   OBJTIME = F$CVTIME("01-JAN-1970 00:00:00.00","COMPARISON")
$ ENDIF
$!
$! Compile AliM1543C.cpp to es40_lsm_AliM1543C.obj
$ IF SRCTIME .GTS. OBJTIME
$ THEN
$   CXX AliM1543C.cpp -
         /DEFINE=(ES40,__USE_STD_IOSTREAM,IDB,LSM'X11_DEF') -
         /INCLUDE=("''ES40_ROOT'/GUI","''ES40_ROOT'/BASE'") -
         /STANDARD=GNU -
         /ARCHITECTURE=HOST -
         /OPTIMIZE=(LEVEL=4,INLINE=SPEED,TUNE=HOST) -
         /OBJECT=es40_lsm_AliM1543C.obj
$ ENDIF
$!
$! Link es40_lsm
$!
$ SAY "Linking es40_lsm..."
$!
$ CXXLINK es40_lsm_AliM1543C.obj'X11_LIB' -
           /EXECUTABLE=es40_lsm.exe
$!
$! Compile sources for es40_cfg
$!
$! Compile with the following defines: ES40,__USE_STD_IOSTREAM
$!
$ SAY "Compiling es40_cfg..."
$!
$! Check if es40_cfg_es40-cfg.obj is up-to-date...
$!
$ SRCTIME = F$CVTIME(F$FILE_ATTRIBUTES("es40-cfg.cpp","RDT"),"COMPARISON")
$ OBJFILE = F$SEARCH("es40_cfg_es40-cfg.obj")
$ IF OBJFILE .NES. ""
$ THEN
$   OBJTIME = F$CVTIME(F$FILE_ATTRIBUTES("es40_cfg_es40-cfg.obj","RDT"),"COMPARISON")
$ ELSE
$   OBJTIME = F$CVTIME("01-JAN-1970 00:00:00.00","COMPARISON")
$ ENDIF
$!
$! Compile es40-cfg.cpp to es40_cfg_es40-cfg.obj
$ IF SRCTIME .GTS. OBJTIME
$ THEN
$   CXX es40-cfg.cpp -
         /DEFINE=(ES40,__USE_STD_IOSTREAM'X11_DEF') -
         /INCLUDE=("''ES40_ROOT'/GUI","''ES40_ROOT'/BASE'") -
         /STANDARD=GNU -
         /ARCHITECTURE=HOST -
         /OPTIMIZE=(LEVEL=4,INLINE=SPEED,TUNE=HOST) -
         /OBJECT=es40_cfg_es40-cfg.obj
$ ENDIF
$!
$! Link es40_cfg
$!
$ SAY "Linking es40_cfg..."
$!
$ CXXLINK es40_cfg_es40-cfg.obj'X11_LIB' -
           /EXECUTABLE=es40_cfg.exe
$!
$ SAY "That's all, folks!"
$!
$ EXIT
