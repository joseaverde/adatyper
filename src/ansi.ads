-------------------------------------------------------------------------------
--                                                                           --
--                              A N S I . A D S                              --
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

limited with Ansi.Surfaces;
private with Interfaces.C;
-- private with Interfaces.C.Pointers;
private with System;


-- This package contains everything to work with ANSI escape sequences for
-- colours, styles, formatting, cursor position...
-- It also declares some functions and procedures to work the terminal and it
-- contains the main surface for the program. This way nothing is written
-- directly onto the screen but onto a surface.
package Ansi is
   
   -- This type is an access type for the surface so every other child package
   -- can use them.
   type Surface is access Ansi.Surfaces.Surface_Type;

   -- This constant contains the first part of every escape code sequence.
   ESC: CONSTANT String := ASCII.ESC & '[';
   
   -- This procedure initializes the package.
   procedure Initialize;
   pragma Inline (Initialize);
   
   -- This function returns the height of the screen.
   function Get_Height return Positive;
   pragma Inline (Get_Height);
   
   -- This function returns the width of the screen.
   function Get_Width  return Positive;
   pragma Inline (Get_Width);

   -- This function returns the main surface.
   function Get_Main_Surface return Surface;
   pragma Inline (Get_Main_Surface);
   
   -- This procedure updates the main surface if it has been resized.
   procedure Update_Main_Surface;

   -- This function is a wrapper for the Ioctl function. It returns True if
   -- either the width or the height or both have changed.
   function Update_Terminal_Size return Boolean;
   pragma Inline (Update_Terminal_Size);
 
private
   
   -- This is the winsize type from sys/ioctl.h library from C. It's used to
   -- get the terminal size.
   type Winsize is
   record
      ws_row   : Interfaces.C.unsigned_short;
      ws_col   : Interfaces.C.unsigned_short;
      ws_xpixel: Interfaces.C.unsigned_short;
      ws_ypixel: Interfaces.C.unsigned_short;
   end record
      with Convention => C;

   -- This function gets the size of the screen.
   function Ioctl (Fd     :     Interfaces.C.int;
                   Request:     Interfaces.C.unsigned_long;
                   Struct : out Winsize)
                   return Interfaces.C.int;
   pragma Import (C, Ioctl, "ioctl");

   -- This constant is found under /usr/include/asm-generic/ioctls.h and is the
   -- request to get the size of the window with Ioctl, its a MACRO so it can't
   -- be imported natively, so I'm writting the value found in the file.
   TIOCGWINSZ: CONSTANT Interfaces.C.unsigned_long := 16#5413#;
  
   -- The dimensions of the screen.
   Width : Positive := 1;
   Height: Positive := 1;
   
   -- The main surface.
   Main_Surface: Surface;

   -- Whether the package has been initialized.
   Is_Initialized: Boolean := False;

   -- This procedure initializes the main surface.
   procedure Create_Main_Surface;
   pragma Inline (Create_Main_Surface);

end Ansi;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
