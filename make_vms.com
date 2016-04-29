$ SET NOVERIFY
$!
$! =============================================================================
$!
$! ES40 emulator.
$!
$! Copyright (C) 2007-2008 by the ES40 Emulator Project
$!
$! Website: http://sourceforge.net/projects/es40
$! E-mail : camiel@camicom.com
$! 
$! This program is free software; you can redistribute it and/or
$! modify it under the terms of the GNU General Public License
$! as published by the Free Software Foundation; either version 2
$! of the License, or (at your option) any later version.
$! 
$! This program is distributed in the hope that it will be useful,
$! but WITHOUT ANY WARRANTY; without even the implied warranty of
$! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
$! GNU General Public License for more details.
$! 
$! You should have received a copy of the GNU General Public License
$! along with this program; if not, write to the Free Software
$! Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 
$! 02110-1301, USA.
$! 
$! Although this is not required, the author would appreciate being notified of, 
$! and receiving any modifications you may make to the source code that might serve
$! the general public.
$!
$! =============================================================================
$!
$! $Id: make_vms.com $
$!
$! X-1.3     Camiel Vanderhoeven                          02-APR-2008
$!   Added SET ON again after SET NOON.
$!
$! X-1.2     Camiel Vanderhoeven                          02-APR-2008
$!   Show a warning if GNV is not installed.
$!
$! X-1.1	 Camiel Vanderhoeven                          31-MAR-2008
$!      File Created.
$!
$! =============================================================================
$!
$ SAY = "WRITE SYS$OUTPUT"
$!
$ SAY "Configuring the ES40 Emulator..."
$!
$ SET NOON
$ BASH -c configure_1.sh
$ IF $STATUS
$ THEN
$   SAY "  Configuration complete."
$ ELSE
$   SAY "  It looks like GNV is not installed."
$   SAY "  Please edit [.SRC]CONFIG_DEBUG.H manually if required,"
$   SAY "  and restart this procedure."
$ ENDIF
$ SET ON
$!
$ SAY ""
$ SAY "Building the ES40 Emulator..."
$!
$ SET DEF [.SRC]
$ @MAKE_VMS.COM
$ SET DEF [-]
$!
$ SAY "  Build complete."
$!
$ EXIT
