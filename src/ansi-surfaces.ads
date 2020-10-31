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


-- This package contains functions to work with surfaces.
package Ansi.Surfaces is

   -- pragma Elaborate_Body (Ansi.Surfaces);
   
   -- The layerer type can hold many layers of Surfaces that can be then sorted
   -- and updated in that order together. This package only contains one.
   type Layerer_Type is tagged limited private;

   ----------------------------
   -- FUNCTIONS FOR SURFACES --
   ----------------------------
   
   -- This procedure prints a string into a surface. If the surface is null, it
   -- points to the main surface. It raises an error if the string goes out of
   -- ranges but it writes it until it can. It also moves the cursor one space
   -- to the left of the place where it ended. (Out_Of_Bounds_Issue)
   procedure Put (Item   : Str_Type;
                  Surface: Surface_Access := null);

   -- This procedure prints a character into a surface. If the surface is null,
   -- it points to the main surface. It raises an error if the character goes
   -- out of bounds. (Out_Of_Bounds_Issue)
   procedure Put (Item   : Char_Type;
                  Surface: Surface_Access := null);

   -- This procedure forces a surface to be printed onto the screen, if it goes
   -- out of bounds it raises an exception Out_Of_Bounds_Issue. If the surface
   -- is null, then it's the main surface.
   -- The Update paramater tell whether to clear the stack and update the
   -- surface.
   procedure Put (Surface: Surface_Access := null;
                  Update : Boolean        := False);

private
   
   type Surface_Array is array (Positive range <>) of Surface_Access;
   type Layer_Array is access Surface_Array;

   -- The layers are stored in an array access that can upgraded in runtime,
   -- the array contains Surface_Access_es in a given order that can be changed
   -- with functions. There is a special surface which is Null which refers to
   -- the main screen and which is always the first one in being displayed and
   -- it's the one where all changes will be printed and the one that will be
   -- printed.
   type Layerer_Type is tagged limited
      record
         -- The array, which by default has only one item.
         Layers: Layer_Array := new Surface_Array(1 .. 1);
      end record;

   The_Layers: Layerer_Type;
end Ansi.Surfaces;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
