################################################################################
# ES40 emulator.
# Copyright (C) 2007-2008 by the ES40 Emulator Project
#
# Website: http://www.es40.org
# E-mail : camiel@es40.org
# 
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 
# 02110-1301, USA.
# 
# Although this is not required, the author would appreciate being notified of, 
# and receiving any modifications you may make to the source code that might serve
# the general public.
#
################################################################################
#
# $Id: make_vms.sh,v 1.5 2008/05/31 15:47:15 iamcamiel Exp $
#
# X-1.5      Camiel Vanderhoeven                      31-MAY-2008
#      Add parts of Poco.
#
# X-1.4      Camiel Vanderhoeven                          02-APR-2008
#   a) Determine ES40 source directory automatically.
#   b) Determine if X11 is available automatically.
#
# X-1.3      Camiel Vanderhoeven                          01-APR-2008
#      Files taken automatically from Makefile.am.
#
# X-1.2	 Camiel Vanderhoeven                          31-MAR-2008
#      VMS-specific files added.
#
# X-1.1	 Camiel Vanderhoeven                          20-MAR-2008
#      File Created.
#
################################################################################

#
# List files here that are only part of the OpenVMS port, and that are not in
# Makefile.am
#

es40_VMS_SOURCES=""

#
# No user serviceable parts beyond this point...
#

es40_CONFIGS="es40 es40_idb es40_lss es40_lsm es40_cfg"

cat > make_vms.com << VMS_EOF
\$ SET NOVERIFY
\$!
\$! ES40 Emulator
\$! Copyright (C) 2007-2008 by the ES40 Emulator Project
\$!
\$! This file was created by make_vms.sh. Please refer to that file
\$! for more information.
\$!
\$ SAY = "WRITE SYS\$OUTPUT"
\$!
\$! DETERMINE ES40 SRC ROOT PATH IN UNIX-STYLE SYNTAX
\$!
\$ DFLT = F\$STRING("/" + F\$ENVIRONMENT("DEFAULT"))
\$ DLEN = F\$LENGTH("''DFLT'")
\$!
\$ loop_dot:
\$   DD = F\$LOCATE(".",DFLT)
\$   IF DD .EQ. DLEN
\$   THEN
\$     GOTO loop_dot_end
\$   ENDIF
\$   DFLT[DD,1]:="/"
\$ GOTO loop_dot
\$ loop_dot_end:
\$!
\$ DD = F\$LOCATE(":[",DFLT)
\$ IF DD .NE. DLEN
\$ THEN
\$   DFLT[DD,2]:="/"
\$ ENDIF
\$!
\$ DD = F\$LOCATE("]",DFLT)
\$ IF DD .NE. DLEN
\$ THEN
\$   DFLT[DD,1]:=""
\$ ENDIF
\$!
\$ DD = F\$LOCATE("\$",DFLT)
\$ IF DD .NE. DLEN
\$ THEN
\$   DFLT=F\$STRING(F\$EXTRACT(0,DD,DFLT) + "\\\$" + F\$EXTRACT(DD+1,DLEN-DD,DFLT))
\$ ENDIF
\$!
\$ ES40_ROOT = F\$EDIT(DFLT,"COLLAPSE")
\$!
\$! Determine if X11 support is available...
\$!
\$ CREATE X11TEST.CPP
\$ DECK
#include <X11/Xlib.h>

void x() { XOpenDisplay(NULL); }
\$ EOD
\$ SET NOON
\$ CXX X11TEST.CPP /OBJECT=X11TEST.OBJ
\$ IF \$STATUS
\$ THEN
\$   SAY "Have found X11 support"
\$   X11_DEF=",HAVE_X11"
\$   X11_LIB=",SYS\$LIBRARY:DECWINDOWS/LIB"
\$ ELSE
\$   SAY "Have not found X11 support"
\$   X11_DEF=""
\$   X11_LIB=""
\$ ENDIF
\$ DELETE X11TEST.CPP;
\$ DELETE X11TEST.OBJ;
\$ SET ON
\$!
VMS_EOF

#
# Read normal sources from Makefile.am
#
es40_REG_SOURCES=`
  for z in "z"; do
    started="no"
    result=""
    while read line_a line_b line_c line_d; do
      if test "$started" = "no" -a "$line_a" = "es40_SOURCES"; then
        started="yes"
        line_a=$line_c
      fi
      if test "$started" = "yes"; then
        if test "X$line_b" = "X"; then
          started="end"
        else
          result="$result $line_a"
        fi
      fi
    done
    echo "$result"
  done < Makefile.am
`
es40_CFG_SOURCES=`
  for z in "z"; do
    started="no"
    result=""
    while read line_a line_b line_c line_d; do
      if test "$started" = "no" -a "$line_a" = "es40_cfg_SOURCES"; then
        started="yes"
        line_a=$line_c
      fi
      if test "$started" = "yes"; then
        if test "X$line_b" = "X"; then
          started="end"
        else
          result="$result $line_a"
        fi
      fi
    done
    echo "$result"
  done < Makefile.am
`

es40_SOURCES="${es40_VMS_SOURCES}${es40_REG_SOURCES}"

for current_CONFIG in $es40_CONFIGS; do

  es40_DEFINES=ES40,__USE_STD_IOSTREAM
  es40_INCLUDE="\"''ES40_ROOT'/GUI\",\"''ES40_ROOT'/BASE'\""
  es40_OPTIMIZE="LEVEL=4,INLINE=SPEED,TUNE=HOST"
  es40_ARCH="HOST"
  es40_STANDARD="GNU"

  if test "$current_CONFIG" = "es40_idb"; then
    es40_DEFINES=$es40_DEFINES,IDB
  elif test "$current_CONFIG" = "es40_lss"; then
    es40_DEFINES=$es40_DEFINES,IDB,LSS
  elif test "$current_CONFIG" = "es40_lsm"; then
    es40_DEFINES=$es40_DEFINES,IDB,LSM
  elif test "$current_CONFIG" = "es40_cfg"; then
    es40_SOURCES="${es40_VMS_SOURCES}${es40_CFG_SOURCES}"
  fi

  cat >> make_vms.com << VMS_EOF
\$!
\$! Compile sources for $current_CONFIG
\$!
\$! Compile with the following defines: $es40_DEFINES
\$!
\$ SAY "Compiling $current_CONFIG..."
VMS_EOF

  es40_OBJECTS=""
  for source_FILE in $es40_SOURCES; do

    if test "${source_FILE:0:4}" = "gui/"; then
      source_FILE=${source_FILE#gui/}
      object_FILE="gui_$source_FILE"
      source_FILE="[.gui]$source_FILE"
    elif test "${source_FILE:0:5}" = "base/"; then
      source_FILE=${source_FILE#base/}
      object_FILE="base_$source_FILE"
      source_FILE="[.base]$source_FILE"
    else
      object_FILE=$source_FILE
    fi

    object_FILE=${current_CONFIG}_${object_FILE%.cpp}.obj

    cat >> make_vms.com << VMS_EOF
\$!
\$! Check if $object_FILE is up-to-date...
\$!
\$ SRCTIME = F\$CVTIME(F\$FILE_ATTRIBUTES("$source_FILE","RDT"),"COMPARISON")
\$ OBJFILE = F\$SEARCH("$object_FILE")
\$ IF OBJFILE .NES. ""
\$ THEN
\$   OBJTIME = F\$CVTIME(F\$FILE_ATTRIBUTES("$object_FILE","RDT"),"COMPARISON")
\$ ELSE
\$   OBJTIME = F\$CVTIME("01-JAN-1970 00:00:00.00","COMPARISON")
\$ ENDIF
\$!
\$! Compile $source_FILE to $object_FILE
\$ IF SRCTIME .GTS. OBJTIME
\$ THEN
\$   CXX $source_FILE -
         /DEFINE=(${es40_DEFINES}'X11_DEF') -
         /INCLUDE=($es40_INCLUDE) -
         /STANDARD=$es40_STANDARD -
         /ARCHITECTURE=$es40_ARCH -
         /OPTIMIZE=($es40_OPTIMIZE) -
         /OBJECT=$object_FILE
\$ ENDIF
VMS_EOF

    if test "X$es40_OBJECTS" = "X"; then
      es40_OBJECTS="$object_FILE"
    else
      es40_OBJECTS="$es40_OBJECTS,$object_FILE"
    fi
  done

  es40_OUTPUT=${current_CONFIG}.exe

  cat >> make_vms.com << VMS_EOF
\$!
\$! Link $current_CONFIG
\$!
\$ SAY "Linking $current_CONFIG..."
\$!
\$ CXXLINK $es40_OBJECTS'X11_LIB' -
           /EXECUTABLE=$es40_OUTPUT
VMS_EOF

done

cat >> make_vms.com << VMS_EOF
\$!
\$ SAY "That's all, folks!"
\$!
\$ EXIT
VMS_EOF


