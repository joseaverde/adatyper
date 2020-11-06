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


-- This package contains procedures to work with styles.
package Ansi.Styles is
   
   ------------------------
   -- SURFACE OPERATIONS --
   ------------------------
   
   -- This procedure returns the style in a certain position of the surface.
   procedure Get_Style (Surface: in  not null Surface_Type;
                        Styles : out Style_Array;
                        Row    : Row_Type;
                        Col    : Col_Type);
   pragma Inline (Get_Style);


   -- This procedure puts a style array in standard output.
   procedure Put_Style (Styles: Style_Array);

   -- This procedure puts a style in standard output.
   procedure Put_Style (Style: Style_Type);
   pragma Inline (Put_Style);


   -- This procedure sets a style array in a cell of the surface.
   procedure Set_Style (Surface: out not null Surface_Type;
                        Styles : Style_Array;
                        Row    : Row_Type;
                        Col    : Col_Type);
   pragma Inline (Set_Style);

   -- This procedure sets a style to true in a cell of the surface.
   procedure Set_Style (Surface: out not null Surface_Type;
                        Style  : Style_Type;
                        Row    : Row_Type;
                        Col    : Col_Type);

   -- This procedure removes a style (sets it to false) in a cell of the
   -- surface.
   procedure Remove_Style (Surface: out not null Surface_Type;
                           Style  : Style_Type;
                           Row    : Row_Type;
                           Col    : Col_Type);


   -- This procedure sets a style array in a block from the surface.
   procedure Set_Style (Surface : out not null Surface_Type;
                        Styles  : Style_Array;
                        From_Row: Row_Type;
                        From_Col: Col_Type;
                        To_Row  : Row_Type;
                        To_Col  : Col_Type);

   -- This procedure sets a style to true in a block from the surface.
   procedure Set_Style (Surface : out not null Surface_Type;
                        Style   : Style_Type;
                        From_Row: Row_Type;
                        From_Col: Col_Type;
                        To_Row  : Row_Type;
                        To_Col  : Col_Type);

   -- This procedure removes a style from a block of the surface.
   procedure Remove_Style (Surface : out not null Surface_Type;
                           Style   : Style_Type;
                           From_Row: Row_Type;
                           From_Col: Col_Type;
                           To_Row  : Row_Type;
                           To_Col  : Col_Type);


   -- This procedure sets a style array for all the surface.
   procedure Set_Style (Surface: out not null Surface_Type;
                        Styles : Style_Array);

   -- This procedure sets a style to true in all cells of the surface.
   procedure Set_Style (Surface: out not null Surface_Type;
                        Style  : Style_Type);

   -- This procedure removes a style from all cells of the surface.
   procedure Remove_Style (Surface: out not null Surface_Type;
                           Style  : Style_Type);


   ---------------------------------
   -- SURFACE'S CURSOR OPERATIONS --
   ---------------------------------

   -- This procedure returns the default style of the cursor.
   procedure Get_Cursor_Style (Surface: in  not null Surface_Type;
                               Styles : out Style_Array);
   pragma Inline (Get_Cursor_Style);

   -- This procedure changes the default styles of the cursor to an array of
   -- them.
   procedure Set_Cursor_Style (Surface: out not null Surface_Type;
                               Styles : in  Style_Array);
   pragma Inline (Set_Cursor_Style);

   -- This procedure changes a default style of the cursor to true.
   procedure Set_Cursor_Style (Surface: out not null Surface_Type;
                               Style  : in  Style_Type);
   pragma Inline (Set_Cursor_Style);

   -- This procedure removes a default style of the cursor.
   procedure Remove_Cursor_Style (Surface: out not null Surface_Type;
                                  Style  : in  Style_Type);
   pragma Inline (Remove_Cursor_Style);

end Ansi.Styles;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
