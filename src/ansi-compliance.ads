-------------------------------------------------------------------------------
--                                                                           --
--                   A N S I - C O M P L I A N C E . A D S                   --
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

--
-- @summary
-- This package is used to define some common functions that depend on whether
-- the terminal/console is ansi-compliant or not.
--
-- @description
-- Instead of having millions of external implementation files for each of the
-- functions and procedures that differ depending on whether the terminal or
-- the console is ansi-compliant or not, we are going to create this private
-- package to handle does parts with conditional compilation. Keep in mind even
-- though the newest versions of Windows10 allow Ansi Escape Sequences, older
-- versions doesn't, that's why I'm separing it (for portability). Also even if
-- Windows isn't POSIX compliant, it will use the implementation found in the
-- `%/src/posix/ansi-compliant' directory. For now there are no implementation
-- for POSIX-compliant non-ANSI-compliant operating systems (I don't even know
-- if they exist)
--
private package Ansi.Compliance is

   ------------------------
   -- COLOURS OPERATIONS --
   ------------------------
   
   --
   -- This function depends on the compliance. If it is ansi-compliant it will
   -- generate the number that must be written between `ESC[' and `m' to put
   -- that same colour in an ansi-compliant terminal. In Windows it will return
   -- a null string `""' and call the function to change it.
   --
   -- @param Color
   -- The colour we want to get the foreground colour sequence from.
   --
   -- @param Bright
   -- The brightness of such colour.
   --
   -- @return
   -- In Windows it returns "" and in others it returns the code to change that
   -- colour without the `ESC[ m' as explained in the description.
   --
   function Gen_Foreground (Color : Color_Type;
                            Bright: Boolean)
                            return Str_Type;
   pragma Inline (Gen_Foreground);
   pragma Pure_Function (Gen_Foreground);

   --
   -- This function depends on the compliance. If it is ansi-compliant it will
   -- generate the number that must be written between `ESC[' and `m' to put
   -- that same colour in an ansi-compliant terminal. In Windows it will return
   -- a null string `""' and call the function to change it.
   --
   -- @param Color
   -- The colour we want to get the background colour sequence from.
   --
   -- @param Bright
   -- The brightness of such colour.
   --
   -- @return
   -- In Windows it returns "" and in others it returns the code to change that
   -- colour without the `ESC[ m' thing as explained in the description.
   --
   function Gen_Background (Color : Color_Type;
                            Bright: Boolean)
                            return Str_Type;
   pragma Inline (Gen_Background);
   pragma Pure_Function (Gen_Background);


   --
   -- This procedure puts a colour into standard output's foreground.
   --
   -- @param Color
   -- The colour to put.
   --
   -- @param Bright
   -- The brightness of such colour.
   --
   procedure Put_Foreground (Color : Color_Type;
                             Bright: Boolean);
   pragma Inline (Put_Foreground);

   --
   -- This procedure puts a colour into standard output's background.
   --
   -- @param Color
   -- The colour to put.
   --
   -- @param Bright
   -- The brightness of such colour.
   --
   procedure Put_Background (Color : Color_Type;
                             Bright: Boolean);
   pragma Inline (Put_Background);


   ----------------------
   -- STYLE OPERATIONS --
   ----------------------
   
   -----------------------
   -- CURSOR OPERATIONS --
   -----------------------

end Ansi.Compliance;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
