-------------------------------------------------------------------------------
--                                                                           --
--                       A N S I - C O L O R S . A D S                       --
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


-- This package contains the functions and procedures to work with colours.
package Ansi.Colors is

   pragma Elaborate_Body (Ansi.Colors);
   
   -- This type declares the avalible colours for a normal terminal or console.
   -- FIXME: It may need some changes for the Windows console.
   type Color_Type is (Black, Red, Green, Yellow, Blue, Magenta, Cyan, White);
   for Color_Type use
      (Black   => 0,
       Red     => 1,
       Green   => 2,
       Yellow  => 3,
       Blue    => 4,
       Magenta => 5,
       Cyan    => 6,
       White   => 7);
   for Color_Type'Size use 3;

   -- This is just a shortcut for Bright.
   Bright: CONSTANT Boolean := True;

   -- This procedure changes the foreground color of the terminal.
   procedure Set_Foreground_Color (Color  : Color_Type;
                                   Bright : Boolean := False);
   
   -- This procedure changes the background color of the terminal.
   procedure Set_Background_Color (Color  : Color_Type;
                                   Bright : Boolean := False);

   -- This procedure is a shortcut for the two above, but it will make both
   -- colours with the same chosen brightness.
   procedure Set_Color (Fg_Color: Color_Type;
                        Bg_Color: Color_Type;
                        Bright  : Boolean := False);
   pragma Inline (Set_Color);

   -- This procedure resets the terminal colour.
   procedure Plain;
   pragma Inline (Plain);

end Ansi.Colors;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
