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

--
-- @summary
-- This package contains the functions and procedures to work with colours.
--
-- @description
-- This package is used to colourise a little bit the output with Ansi Escape
-- Sequences which is what this library is all about. The implementation is
-- separated because colourising in windows it's different than in linux.
--
package Ansi.Colors is

   ------------------------
   -- SURFACE OPERATIONS --
   ------------------------

   --
   -- This function generates the escape code for the foreground colour.
   -- Only the number, if the escape code is "ESC[32m", then 32 is returned.
   --
   -- @param Color
   -- The colour of the foreground you want to get the escape code from.
   --
   -- @param Bright
   -- Whether you want the colour to be bright or not.
   --
   -- @return
   -- It returns the escape code sequence used to colour the terminal. But it
   -- only returns the number, for example if the whole sequence is `ESC[32m',
   -- then `32' is returned.
   --
   function Gen_Foreground (Color : Color_Type;
                            Bright: Boolean)
                            return Str_Type;
   pragma Pure_Function (Gen_Foreground);
   pragma Inline (Gen_Foreground);

   --
   -- This function generates the escape code number for the background colour.
   --
   -- @param Color
   -- The colour of the background you want to get the escape code from.
   --
   -- @param Bright
   -- Whether you want the colour to be bright or not.
   --
   -- @return
   -- It returns the escape code sequence used to colour the terminal. But it
   -- returns only the number, for example if the whose sequence is `ESC[45m',
   -- then `45' is returned.
   --
   function Gen_Background (Color : Color_Type;
                            Bright: Boolean)
                            return Str_Type;
   pragma Pure_Function (Gen_Background);
   pragma Inline (Gen_Background);


   --
   -- This procedure returns the foreground colour in a certain position of the
   -- surface.
   --
   -- @param Surface
   -- The surface to get the foreground colour from.
   --
   -- @param Color
   -- The returned foreground colour from the surface.
   --
   -- @param Bright
   -- The returned foreground colour's brightness from the surface.
   --
   -- @param Row
   -- The row of the cell whose colour you want to retrieve.
   --
   -- @param Col
   -- The column of the cell whose colour you want to retrieve.
   --
   -- @exception Ansi.Exceptions.Out_Of_Bounds_Error
   -- This exception is raised if the position is out of the surface's bounds.
   --
   procedure Get_Foreground (Surface: in  not null Surface_Type;
                             Color  : out Color_Type;
                             Bright : out Boolean;
                             Row    :     Row_Type;
                             Col    :     Col_Type);

   --
   -- This procedure returns the background colour in a certain position of the
   -- surface.
   --
   -- @param Surface
   -- The surface from which the background colour will be retrieved.
   --
   -- @param Color
   -- The colour of the given position.
   --
   -- @param Bright
   -- The brightness of the colour.
   --
   -- @param Row
   -- The row where the colour is found.
   --
   -- @param Col
   -- The column where the colour is found.
   --
   -- @exception Ansi.Exceptions.Out_Of_Bounds_Issue
   -- This exceptions is raised if the colour is out of the surface's bounds.
   --
   procedure Get_Background (Surface: in  not null Surface_Type;
                             Color  : out Color_Type;
                             Bright : out Boolean;
                             Row    :     Row_Type;
                             Col    :     Col_Type);


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

   -- This procedure puts a colour into standard output.
   --
   -- @param Color
   -- The colour to put.
   --
   -- @param Bright
   -- The brightness of such colour.
   --
   procedure Put_Background (Color : Color_Type;
                             Bright: Boolean);


   --
   -- This procedure sets a foreground colour in a cell of the surface.
   --
   -- @param Surface
   -- The surface to edit.
   --
   -- @param Color
   -- The colour we want to put in the foreground.
   --
   -- @param Bright
   -- The brightness of such colour.
   --
   -- @param Row
   -- The row of the surface.
   --
   -- @param Col
   -- The column of the surface.
   --
   -- @exception Ansi.Exceptions.Out_Of_Bounds_Issue
   -- This exception is raised if the position is out of the surface's bounds.
   --
   procedure Set_Foreground (Surface: in not null Surface_Type;
                             Color  : Color_Type;
                             Bright : Boolean;
                             Row    : Row_Type;
                             Col    : Col_Type);

   --
   -- This procedure sets a background colour in a cell of the surface.
   -- 
   -- @param Surface
   -- The surface to edit.
   --
   -- @param Color
   -- The colour we want to put in the background.
   --
   -- @param Bright
   -- The brightness of such colour.
   --
   -- @param Row
   -- The row of the surface.
   --
   -- @param Col
   -- The column of the surface.
   --
   -- @exception Ansi.Exceptions.Out_Of_Bounds_Issue
   -- This exception is raised if the position is out of the surface's bounds.
   --
   procedure Set_Background (Surface: in not null Surface_Type;
                             Color  : Color_Type;
                             Bright : Boolean;
                             Row    : Row_Type;
                             Col    : Col_Type);

   --
   -- This procedure sets a foreground colour in a block of cells from the
   -- surface.
   --
   -- @param Surface
   -- The surface to edit.
   --
   -- @param Color
   -- The colour we want to put in the foreground.
   --
   -- @param Bright
   -- The brightness of such colour.
   --
   -- @param From_Row
   -- The row of the surface where the block starts at.
   --
   -- @param From_Col
   -- The column of the surface where the block starts at.
   --
   -- @param To_Row
   -- The row of the surface where the block ends at.
   --
   -- @param To_Col
   -- The column of the surface where the block ends at.
   --
   -- @exception Ansi.Exceptions.Out_Of_Bounds_Issue
   -- This exception is raised in 3 different cases:
   --    - If the `from' position is out of bounds.
   --    - If the `to' position is out of bounds.
   --    - If the `from' position is greater than the `to' one.
   --
   procedure Set_Foreground (Surface : in not null Surface_Type;
                             Color   : Color_Type;
                             Bright  : Boolean;
                             From_Row: Row_Type;
                             From_Col: Col_Type;
                             To_Row  : Row_Type;
                             To_Col  : Col_Type)
                             with Pre => From_Row <= To_Row and
                                         From_Col <= To_Col;

   --
   -- This procedure sets a background colour in a block of cells from the
   -- surface.
   --
   -- @param Surface
   -- The surface to edit.
   --
   -- @param Color
   -- The colour we want to put in the background.
   --
   -- @param Bright
   -- The brightness of such colour.
   --
   -- @param From_Row
   -- The row of the surface where the block starts at.
   --
   -- @param From_Col
   -- The column of the surface where the block starts at.
   --
   -- @param To_Row
   -- The row of the surface where the block ends at.
   --
   -- @param To_Col
   -- The column of the surface where the block ends at.
   --
   -- @exception Ansi.Exceptions.Out_Of_Bounds_Issue
   -- This exception is raised in 3 different cases:
   --    - If the `from' position is out of bounds.
   --    - If the `to' position is out of bounds.
   --    - If the `from' position is greater than the `to' one.
   --
   procedure Set_Background (Surface : in not null Surface_Type;
                             Color   : Color_Type;
                             Bright  : Boolean;
                             From_Row: Row_Type;
                             From_Col: Col_Type;
                             To_Row  : Row_Type;
                             To_Col  : Col_Type)
                             with Pre => From_Row <= To_Row and
                                         From_Col <= To_Col;

   --
   -- This procedure sets the foreground colour of a whole surface.
   --
   -- @param Surface
   -- The surface to edit.
   --
   -- @param Color
   -- The foreground colour of the surface.
   --
   -- @param Bright
   -- The brightness of the colour.
   --
   procedure Set_Foreground (Surface: not null Surface_Type;
                             Color  : Color_Type;
                             Bright : Boolean);
   
   --
   -- This procedure sets the background colour of a whole surface.
   --
   -- @param Surface
   -- The surface to edit.
   --
   -- @param Color
   -- The background colour of the surface.
   --
   -- @param Bright
   -- The brightness of the colour.
   --
   procedure Set_Background (Surface: not null Surface_Type;
                             Color  : Color_Type;
                             Bright : Boolean);


   ---------------------------------
   -- SURFACE'S CURSOR OPERATIONS --
   ---------------------------------

   --
   -- This procedure returns the default foreground colour from the cursor.
   --
   -- @param Surface
   -- The surface from which we want to retrieve the cursor information.
   --
   -- @param Color
   -- The foreground colour of the given surface's cursor.
   --
   -- @param Bright
   -- The brightness of such colour.
   --
   procedure Get_Cursor_Foreground (Surface: in  not null Surface_Type;
                                    Color  : out Color_Type;
                                    Bright : out Boolean);

   --
   -- This procedure returns the default background colour from the cursor.
   --
   -- @param Surface
   -- The surface from which we want to retrieve the cursor information.
   --
   -- @param Color
   -- The background colour of the given surface's cursor.
   --
   -- @param Bright
   -- The brightness of such colour.
   --
   procedure Get_Cursor_Background (Surface: in  not null Surface_Type;
                                    Color  : out Color_Type;
                                    Bright : out Boolean);

   --
   -- This procedure changes the default foreground colour for the cursor.
   --
   -- @param Surface
   -- The surface where we want to change the cursor information.
   --
   -- @param Color
   -- The foreground colour we want to give.
   --
   -- @param Bright
   -- The brightness of such colour.
   --
   procedure Set_Cursor_Foreground (Surface: not null Surface_Type;
                                    Color  : Color_Type;
                                    Bright : Boolean);

   --
   -- This procedure changes the default background colour for the cursor.
   --
   -- @param Surface
   -- The surface where we want to change the cursor information.
   --
   -- @param Color
   -- The background colour we want to give.
   --
   -- @param Bright
   -- The brightness of such colour.
   --
   procedure Set_Cursor_Background (Surface: not null Surface_Type;
                                    Color  : Color_Type;
                                    Bright : Boolean);

end Ansi.Colors;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
