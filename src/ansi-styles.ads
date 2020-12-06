-------------------------------------------------------------------------------
--                                                                           --
--                       A N S I - S T Y L E S . A D S                       --
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
-- This package contains procedures to work with styles.
--
-- @description
-- This package is used to work with styles or attributes like: bright/bold,
-- underlined, blink, italics and reversed. However in windows the only ones
-- available are Underlined and Bright for the CMD, thus part of the code is
-- compiled conditionaly. All the types are declared at top level Ansi.
--
package Ansi.Styles is
   
   ------------------------
   -- SURFACE OPERATIONS --
   ------------------------
   
   --
   -- This function returns the code used to put a style in a two byte string,
   -- for example to put it bright its: ESC[1m, then it returns "_1" (The
   -- underscore is a null byte. In windows it returns a null string.
   --
   -- @param Style
   -- The style to put.
   --
   -- @param Remove
   -- If true it returns the code to remove the style.
   --
   -- @return
   -- The string to put to get such style.
   --
   function Gen_Style (Style : Style_Type;
                       Remove: Boolean := False)
                       return Str_Type;
   pragma Inline (Gen_Style);
   pragma Pure_Function (Gen_Style);

   --
   -- This procedure returns the style in a certain position of the surface.
   --
   -- @param Surface
   -- The surface where we search the information.
   --
   -- @param Styles
   -- The styles in the given position.
   --
   -- @param Row
   -- The row to search the styles at.
   --
   -- @param Col
   -- The column to search the styles at.
   --
   -- @exceptions Ansi.Exceptions.Out_Of_Bounds_Issue
   -- This exception is raised if either the row or the column or both are out
   -- of the Surface's bounds.
   --
   procedure Get_Style (Surface: in  not null Surface_Type;
                        Styles : out Style_Array;
                        Row    : Row_Type;
                        Col    : Col_Type);


   --
   -- This procedure puts a style array in standard output.
   --
   -- @param Styles
   -- The styles to put.
   --
   procedure Put_Style (Styles: Style_Array);

   --
   -- This procedure puts a style in standard output.
   -- 
   -- @param Style
   -- The style to put into standard output.
   --
   procedure Put_Style (Style: Style_Type);


   -- 
   -- This procedure sets a style array in a cell of the surface.
   -- 
   -- @param Surface
   -- The surface to edit.
   --
   -- @param Styles
   -- The array of styles to set.
   --
   -- @param Row
   -- The row to modify.
   --
   -- @param Col
   -- The column to modify.
   --
   -- @exception Ansi.Exceptions.Out_Of_Bounds_Issue
   -- This exception is raised when the index is out of the surface's bounds.
   --
   procedure Set_Style (Surface: in not null Surface_Type;
                        Styles : Style_Array;
                        Row    : Row_Type;
                        Col    : Col_Type);

   -- This procedure sets a style to true in a cell of the surface.
   --
   -- @param Surface
   -- The surface to edit.
   --
   -- @param Style
   -- The style to set True.
   --
   -- @param Row
   -- The row to modify.
   --
   -- @param Col
   -- The column to modify.
   --
   -- @exception Ansi.Exceptions.Out_Of_Bounds_Issue
   -- This exception is raised when the index is out of the surface's bounds.
   --
   procedure Set_Style (Surface: in not null Surface_Type;
                        Style  : Style_Type;
                        Row    : Row_Type;
                        Col    : Col_Type);

   -- This procedure removes a style (sets it to false) in a cell of the
   -- surface.
   --
   -- @param Surface
   -- The surface to edit.
   --
   -- @param Style
   -- The style to remove.
   --
   -- @param Row
   -- The row to modify.
   --
   -- @param Col
   -- The column to modify.
   --
   -- @exception Ansi.Exceptions.Out_Of_Bounds_Issue
   -- This exception is raised when the index is out of the surface's bounds.
   --
   procedure Remove_Style (Surface: in not null Surface_Type;
                           Style  : Style_Type;
                           Row    : Row_Type;
                           Col    : Col_Type);


   --
   -- This procedure sets a style array in a block from the surface.
   --
   -- @param Surface
   -- The surface to modify.
   --
   -- @param Styles
   -- The styles to set.
   --
   -- @param From_Row
   -- The row where the block starts.
   --
   -- @param From_Col
   -- The column where the block starts.
   --
   -- @param To_Row
   -- The row where the block ends.
   --
   -- @param To_Col
   -- The column where the block ends.
   --
   -- @exception Ansi.Exceptions.Out_Of_Bounds_Issue
   -- This exception is raised when either the `from' index or the `to' or both
   -- are out of the surface's bounds.
   --
   procedure Set_Style (Surface : in not null Surface_Type;
                        Styles  : Style_Array;
                        From_Row: Row_Type;
                        From_Col: Col_Type;
                        To_Row  : Row_Type;
                        To_Col  : Col_Type);

   --
   -- This procedure sets a style to true in a block from the surface.
   --
   -- @param Surface
   -- The surface to modify.
   --
   -- @param Style
   -- The style to set.
   --
   -- @param From_Row
   -- The row where the block starts.
   --
   -- @param From_Col
   -- The column where the block starts.
   --
   -- @param To_Row
   -- The row where the block ends.
   --
   -- @param To_Col
   -- The column where the block ends.
   --
   -- @exception Ansi.Exceptions.Out_Of_Bounds_Issue
   -- This exception is raised when either the `from' index or the `to' or both
   -- are out of the surface's bounds.
   --
   procedure Set_Style (Surface: in not null Surface_Type;
                        Style   : Style_Type;
                        From_Row: Row_Type;
                        From_Col: Col_Type;
                        To_Row  : Row_Type;
                        To_Col  : Col_Type);

   --
   -- This procedure removes a style from a block of the surface.
   --
   -- @param Surface
   -- The surface to modify.
   --
   -- @param Style
   -- The style to remove.
   --
   -- @param From_Row
   -- The row where the block starts.
   --
   -- @param From_Col
   -- The column where the block starts.
   --
   -- @param To_Row
   -- The row where the block ends.
   --
   -- @param To_Col
   -- The column where the block ends.
   --
   -- @exception Ansi.Exceptions.Out_Of_Bounds_Issue
   -- This exception is raised when either the `from' index or the `to' or both
   -- are out of the surface's bounds.
   --
   procedure Remove_Style (Surface: in not null Surface_Type;
                           Style   : Style_Type;
                           From_Row: Row_Type;
                           From_Col: Col_Type;
                           To_Row  : Row_Type;
                           To_Col  : Col_Type);


   --
   -- This procedure sets a style array for all cells of the surface.
   --
   -- @param Surface
   -- The surface to fill with styles.
   --
   -- @param Styles
   -- The styles to set.
   --
   procedure Set_Style (Surface: in not null Surface_Type;
                        Styles : Style_Array);

   --
   -- This procedure sets a style to true in all cells of the surface.
   --
   -- @param Surface
   -- The surface to fill with styles.
   --
   -- @param Style
   -- The style to set.
   --
   procedure Set_Style (Surface: in not null Surface_Type;
                        Style  : Style_Type);

   --
   -- This procedure removes a style from all cells of the surface.
   --
   -- @param Surface
   -- The surface to remove styles from.
   --
   -- @param Style
   -- The style to remove.
   --
   procedure Remove_Style (Surface: in not null Surface_Type;
                           Style  : Style_Type);


   ---------------------------------
   -- SURFACE'S CURSOR OPERATIONS --
   ---------------------------------

   --
   -- This procedure returns the default style of the cursor.
   --
   -- @param Surface
   -- The surface to retrieve the cursor from.
   --
   -- @param Styles
   -- The styles in the cursor.
   --
   procedure Get_Cursor_Style (Surface: in  not null Surface_Type;
                               Styles : out Style_Array);

   --
   -- This procedure changes the default styles of the cursor to an array of
   -- them.
   --
   -- @param Surface
   -- The surface whose cursor we want to modify.
   --
   -- @param Styles
   -- The styles to set.
   --
   procedure Set_Cursor_Style (Surface: in not null Surface_Type;
                               Styles : in  Style_Array);

   --
   -- This procedure changes a default style of the cursor to true.
   --
   -- @param Surface
   -- The surface whose cursor we want to modify.
   --
   -- @param Style
   -- The style we want to set.
   --
   procedure Set_Cursor_Style (Surface: in not null Surface_Type;
                               Style  : in  Style_Type);

   --
   -- This procedure removes a default style of the cursor.
   --
   -- @param Surface
   -- The surface whose cursor we want to modify.
   --
   -- @param Style
   -- The style we want to remove.
   --
   procedure Remove_Cursor_Style (Surface: in not null Surface_Type;
                                  Style  : in  Style_Type);

end Ansi.Styles;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
