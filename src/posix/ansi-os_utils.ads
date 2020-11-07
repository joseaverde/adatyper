-------------------------------------------------------------------------------
--                                                                           --
--                     A N S I - O S _ U T I L S . A D S                     --
--                                 P O S I X                                 --
--                                                                           --
--                              A D A T Y P E R                              --
--                                                                           --
--                                  S P E C                                  --
--                                                                           --
-------------------------------------------------------------------------------
--     Copyright (c) 2020 José Antonio Verde Jiménez All Rights Reserved     --
-------------------------------------------------------------------------------
-- This file is part of adatyper.                                            --
--                                                                           --
-- This program is free software:  you  can redistribute it and/or modify it --
-- under  the terms  of the  GNU  General License  as published by the  Free --
-- Software  Foundation,  either  version 3  of  the  License,  or  (at your --
-- opinion) any later version.                                               --
--                                                                           --
-- This  program  is distributed  in the  hope that  it will be  useful, but --
-- WITHOUT   ANY   WARRANTY;   without   even  the   implied   warranty   of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General --
-- Public License for more details.                                          --
--                                                                           --
-- You should have received  a copy of the  GNU General Public License along --
-- with this program. If not, see <https://www.gnu.org/licenses/>.           --
--                                                                           --
-------------------------------------------------------------------------------

with Interfaces.C;

-- This package contains some os specific functions from C in the POSIX
-- operating systems like GNU/Linux.
private package Ansi.Os_Utils is

   ------------
   -- SYSTEM --
   ------------
   
   -- This function is used to run a system command.
   function C_System (Cmd: Interfaces.C.Char_Array)
                     return Integer;
   pragma Import (C, C_System, "system");

   -- This function is used to run a system command.
   function System_Command (Cmd: String)
                            return Boolean;


   --------------
   -- TERMINAL --
   --------------
   -- This functions are used to prepare and finalize the terminal, it uses
   -- commands from the OS, so they must be installed beforehand. I suppose
   -- they are preinstalled.
   -- The following two procedures aren't inlined because they must be runned
   -- once.
   
   -- This procedure prepares the terminal.
   procedure Prepare;

   -- This procedure cleans up and restores the terminal.
   procedure Clean_Up;
   
   -----------
   -- IOCTL --
   -----------
   
   -- This is the winsize type from <sys/ioctl.h> library from C. It's used to
   -- store the terminal size.
   type Winsize is
      record
         ws_row   : Interfaces.C.unsigned_short;
         ws_col   : Interfaces.C.unsigned_short;
         ws_xpixel: Interfaces.C.Unsigned_short;
         ws_ypixel: Interfaces.C.Unsigned_short;
      end record
   with Convention => C;
   
   -- The Ioctl function is used to get the size of the terminal.
   function Ioctl (Fd     : Interfaces.C.int;
                   Request: Interfaces.C.unsigned_long;
                   Struct : out Winsize)
                   return Interfaces.C.int;
   pragma Import (C, Ioctl, "ioctl");

   -- This constant is found under </usr/include/asm-generic/ioctls.h> and is
   -- the request to get the size of the window with Ioctls, it's a MACRO so it
   -- can't be imported natively, so I'm writting the value found file.
   TIOCGWINSZ: CONSTANT Interfaces.C.unsigned_long := 16#5413#;

   -- This function updates the terminal size. It's a kind of wrapper for the
   -- ioctl function from C. It returns a Boolean with True if the screen has
   -- been updated.
   function Update_Terminal_Size return Boolean;
   pragma Inline (Update_Terminal_Size);


end Ansi.Os_Utils;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
