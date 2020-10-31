-------------------------------------------------------------------------------
--                                                                           --
--                     A N S I - S U R F A C E S . A D S                     --
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


with Ansi.Colors;
with Ansi.Styles;

-- This package contains surfaces, which are objects where you can write and
-- then print onto the screen minimizing changes.
package Ansi.Surfaces is

   -- pragma Elaborate_Body (Ansi.Surfaces);

   type Surface_Type (Width, Height: Positive) is tagged private;


private

   subtype Color_Type is Ansi.Colors.Color_Type;
   subtype Style_Type is Ansi.Styles.Style_Type;

   -- This type is used to store every character on screen.
   type Item_Type (Is_Formatted: Boolean := False) is
   record
      Char: Wide_Character := Wide_Character'Val(0);
      case Is_Formatted is
         when True =>
            Color: Ansi.Colors.Color_Type;
            Style: Ansi.Styles.Style_Type;
         when others =>
            null;
      end case;
   end record;
   pragma Pack (Item_Type);

   type Matrix_Type is array (Positive range<>, Positive range<>) of Item_Type;

   type Surface_Type (Width, Height: Positive) is tagged
   record

      Matrix: Matrix_Type (1 .. Width, 1 .. Height);
      -- CURSOR
      
   end record;

end Ansi.Surfaces;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
