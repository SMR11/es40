#! /bin/sh

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
# $Id: configure_1.m4,v 1.7 2008/05/31 15:47:07 iamcamiel Exp $
#
# X-1.7      Camiel Vanderhoeven                      31-MAY-2008
#      Add parts of Poco.
#
# X-1.6      Camiel Vanderhoeven                          29-APR-2008
#     Added CHECK_MEM_RANGES and DUMP_MEMMAP
#
# X-1.5      Camiel Vanderhoeven                          03-APR-2008
#     Fixed typo
#
# X-1.4      Camiel Vanderhoeven                          03-APR-2008
#     Added MIPS_ESTIMATE and HAVE_NEW_FP
#
# X-1.3      Camiel Vanderhoeven                          02-APR-2008
#     More questions, better structure.
#
################################################################################

define(ES_ASK_YNS,
# ask a higher-level question, with a response of yes, no, or some
# arg 1: question
# arg 2: default value
# arg 3: action on yes
# arg 4: action on no
# arg 5: action on some
# arg 6: pre-determined answer (if not "", the answer will be assumed to be this)
# arg 7: explanation
  if test "X$6" = "X"; then
    if test "$all_default" = "yes"; then
      answer="$2"
    else
      while true; do
        echo -n "$1? (yes, no, some) [$2]: "
        read answer
        if test "X$answer" = "X"; then
          answer="$2"
        fi
        if test "$answer" = "y" -o "$answer" = "ye" -o "$answer" = "yes"; then
          answer="yes"
          break
        elif test "$answer" = "n" -o "$answer" = "no"; then
          answer="no"
          break
        elif test "$answer" = "s" -o "$answer" = "so" -o "$answer" = "som" -o "$answer" = "some"; then
          answer="some"
          break
        fi
        echo "Invalid value: please answer yes no or some"
      done
    fi
  else
    answer="$6"
  fi
  if test "$answer" = "yes"; then
    ifelse($3, , :, $3)
  elif test "$answer" = "no"; then
    ifelse($4, , :, $4)
  elif test "$answer" = "some"; then
    ifelse($5, , :, $5)
  fi
)

define(ES_ASK_YN,
# ask a question with a response of yes or no
# arg 1: question
# arg 2: default value
# arg 3: action on yes
# arg 4: action on no
# arg 5: pre-determined answer (if not "", the answer will be assumed to be this)
# arg 6: explanation
  if test "X$5" = "X"; then
    if test "$all_default" = "yes"; then
      answer="$2"
    else
      while true; do
        echo -n "$1? (yes, no) [$2]: "
        read answer
        if test "X$answer" = "X"; then
          answer="$2"
        fi
        if test "$answer" = "y" -o "$answer" = "ye" -o "$answer" = "yes"; then
          answer="yes"
          break
        elif test "$answer" = "n" -o "$answer" = "no"; then
          answer="no"
          break
        fi
        echo "Invalid value: please answer yes or no"
      done
    fi
  else
    answer="$5"
  fi
  if test "$answer" = "yes"; then
    ifelse($3, , :, $3)
  elif test "$answer" = "no"; then
    ifelse($4, , :, $4)
  fi
)

define(ES_ASK_DEF,
# ask a yes/no question, and define/undefine a macro accordingly
# arg 1: question to follow "Do you want to "
# arg 2: name of macro
# arg 3: default value
# arg 4: pre-determined answer
# arg 5: explanation
# arg 6: reverse (yes, no)
  ES_ASK_YN(Do you want to $1, $3, debug="yes", debug="no", $4, $5)
  if test "$6" = "yes"; then
    check_for="no"
  else
    check_for="yes"
  fi
  if test "$debug" = $check_for; then
    cat >>src/config_debug.h <<EOF

// Define to 1 if you want to $1
#define $2 1
EOF
  else
    cat >>src/config_debug.h <<EOF

// Define to 1 if you want to $1
#undef $2
EOF
  fi
)

define(ES_ASK_DEBUG,
# arg 1: human-readable name of debugging
# arg 2: name of debug-macro (excluding DEBUG_)
# arg 3: pre-determined answer
  ES_ASK_DEF(enable $1 debugging, DEBUG_$2, no, $3)
)

define(ES_DEBUG_Q, 
  cat >src/config_debug.h <<EOF
// This is config_debug.h
//
// This file contains the debug configuration options.
// This file was generated by configure_1.sh
//
// \$Id\$
EOF
  ES_ASK_YN(Do you want the defaults for all options, yes, all_default="yes", all_default="no")

  ES_ASK_DEF(show the cycle counter, HIDE_COUNTER, no, , , yes)
  ES_ASK_DEF(show estimate speed, MIPS_ESTIMATE, no)
  ES_ASK_DEF(show memory map, DUMP_MEMMAP, no)

  ES_ASK_DEF(check for overlapping of memory ranges, CHECK_MEM_RANGES, yes)

  ES_ASK_DEF(use the new floating-point implementation, HAVE_NEW_FP, no)

  ES_ASK_DEBUG(VGA, VGA)
  ES_ASK_DEBUG(Serial Port, SERIAL)
    
  ES_ASK_YNS(Do you want to enable IDE debugging options, no, ide="yes", ide="no", ide="")
    ES_ASK_DEBUG(IDE General, IDE, $ide)
    ES_ASK_DEBUG(IDE Busmaster, IDE_BUSMASTER, $ide)
    ES_ASK_DEBUG(IDE Command, IDE_COMMAND, $ide)
    ES_ASK_DEBUG(IDE CMD, IDE_CMD, $ide)
    ES_ASK_DEBUG(IDE DMA, IDE_DMA, $ide)
    ES_ASK_DEBUG(IDE Interrupt, IDE_INTERRUPT, $ide)
    ES_ASK_DEBUG(IDE Command Register, IDE_REG_COMMAND, $ide)
    ES_ASK_DEBUG(IDE Control Register, IDE_REG_CONTROL, $ide)
    ES_ASK_DEBUG(IDE ATAPI Packet, IDE_PACKET, $ide)
    ES_ASK_DEBUG(IDE Thread, IDE_THREADS, $ide)
    ES_ASK_DEBUG(IDE Mutexes, IDE_LOCKS, $ide)
    ES_ASK_DEBUG(IDE Multiple, IDE_MULTIPLE, $ide)

  ES_ASK_YNS(Do you want to enable Floating-point debugging options, no, fp="yes", fp="no", fp="")
    ES_ASK_DEBUG(Floating Point conversions, FP_CONVERSION, $fp)
    ES_ASK_DEBUG(Floating Point load/store, FP_LOADSTORE, $fp)

  ES_ASK_YNS(Do you want to enable network interface debugging options, no, nic="yes", nic="no", nic="")
    ES_ASK_DEBUG(General NIC, NIC, $nic)
    ES_ASK_DEBUG(NIC Filter, NIC_FILTER, $nic)
    ES_ASK_DEBUG(NIC Serial ROM, NIC_SROM, $nic)

  ES_ASK_DEBUG(unknown memory access, UNKMEM)
  ES_ASK_DEBUG(PCI, PCI)
  ES_ASK_DEBUG(Translationbuffer, TB)
  ES_ASK_DEBUG(I/O Port Access, PORTACCESS)
  ES_ASK_DEBUG(Keyboard, KBD)
  ES_ASK_DEBUG(Programmable Interrupt Controller (PIC), PIC)
  ES_ASK_DEBUG(Printer port, LPT)
  ES_ASK_DEBUG(USB Controller, USB)
 
  ES_ASK_YNS(Do ypu want to enable SCSI debugging options, no, scsi="yes", scsi="no", scsi="")
    ES_ASK_DEBUG(SCSI Device, SCSI, $scsi)
    ES_ASK_DEBUG(Symbios SCSI Controller, SYM, $scsi)
    ES_ASK_DEBUG(Symbios Registers, SYM_REGS, $scsi)
    ES_ASK_DEBUG(Symbios SCRIPTS Execution, SYM_SCRIPTS, $scsi)

  ES_ASK_DEBUG(DMA Controller, DMA)
  ES_ASK_DEBUG(backtrace on SIGSEGV, BACKTRACE)
  ES_ASK_DEBUG(mutex, LOCKS)
  ES_ASK_DEBUG(SDL Key translation, SDL_KEY)
)
#! /bin/sh
#
# Configure debugging options for es40.
#
echo "This is the debug-options configuration script for the ES40 emulator"
echo "If you don't want any debugging options enabled, answer YES to the"
echo "following question"
echo ""
ES_DEBUG_Q
exit
