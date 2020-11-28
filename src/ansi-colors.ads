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

   ------------------------
   -- SURFACE OPERATIONS --
   ------------------------

   -- This function generates the escape code for the foreground colour.
   -- Only the number, if the escape code is "ESC[32m", then 32 is returned.
   function Gen_Foreground (Color : Color_Type;
                            Bright: Boolean)
                            return Str_Type;
   pragma Pure_Function (Gen_Foreground);
   pragma Inline (Gen_Foreground);

   -- This function generates the escape code number for the background colour.
   function Gen_Background (Color : Color_Type;
                            Bright: Boolean)
                            return Str_Type;
   pragma Pure_Function (Gen_Background);
   pragma Inline (Gen_Background);


   -- This procedure returns the foreground colour in a certain position of the
   -- surface.
   procedure Get_Foreground (Surface: in  not null Surface_Type;
                             Color  : out Color_Type;
                             Bright : out Boolean;
                             Row    :     Row_Type;
                             Col    :     Col_Type);
   pragma Inline (Get_Foreground);

   -- This procedure returns the background colour in a certain position of the
   -- surface.
   procedure Get_Background (Surface: in  not null Surface_Type;
                             Color  : out Color_Type;
                             Bright : out Boolean;
                             Row    :     Row_Type;
                             Col    :     Col_Type);
   pragma Inline (Get_Background);


   -- This procedure puts a colour into standard output.
   procedure Put_Foreground (Color : Color_Type;
                             Bright: Boolean);
   pragma Inline (Put_Foreground);

   -- This procedure puts a colour into standard output.
   procedure Put_Background (Color : Color_Type;
                             Bright: Boolean);
   pragma Inline (Put_Background);

   -- This procedure sets a foreground colour in a cell of the surface.
   procedure Set_Foreground (Surface: in not null Surface_Type;
                             Color  : Color_Type;
                             Bright : Boolean;
                             Row    : Row_Type;
                             Col    : Col_Type);

   -- This procedure sets a background colour in a cell of the surface.
   procedure Set_Background (Surface: in not null Surface_Type;
                             Color  : Color_Type;
                             Bright : Boolean;
                             Row    : Row_Type;
                             Col    : Col_Type);

   -- This procedure sets a foreground colour in a block of cells from the
   -- surface.
   procedure Set_Foreground (Surface : in not null Surface_Type;
                             Color   : Color_Type;
                             Bright  : Boolean;
                             From_Row: Row_Type;
                             From_Col: Col_Type;
                             To_Row  : Row_Type;
                             To_Col  : Col_Type);

   -- This procedure sets a background colour in a block of cells from the
   -- surface.
   procedure Set_Background (Surface : in not null Surface_Type;
                             Color   : Color_Type;
                             Bright  : Boolean;
                             From_Row: Row_Type;
                             From_Col: Col_Type;
                             To_Row  : Row_Type;
                             To_Col  : Col_Type);

   -- This procedure sets the foreground colour of a whole surface.
   procedure Set_Foreground (Surface: not null Surface_Type;
                             Color  : Color_Type;
                             Bright : Boolean);
   
   -- This procedure sets the background colour of a whole surface.
   procedure Set_Background (Surface: not null Surface_Type;
                             Color  : Color_Type;
                             Bright : Boolean);


   ---------------------------------
   -- SURFACE'S CURSOR OPERATIONS --
   ---------------------------------

   -- This procedure returns the default foreground colour from the cursor.
   procedure Get_Cursor_Foreground (Surface: in  not null Surface_Type;
                                    Color  : out Color_Type;
                                    Bright : out Boolean);
   pragma Inline (Get_Cursor_Foreground);

   -- This procedure returns the default background colour from the cursor.
   procedure Get_Cursor_Background (Surface: in  not null Surface_Type;
                                    Color  : out Color_Type;
                                    Bright : out Boolean);
   pragma Inline (Get_Cursor_Background);

   -- This procedure changes the default foreground colour for the cursor.
   procedure Set_Cursor_Foreground (Surface: not null Surface_Type;
                                    Color  : Color_Type;
                                    Bright : Boolean);
   pragma Inline (Set_Cursor_Foreground);

   -- This procedure changes the default background colour for the cursor.
   procedure Set_Cursor_Background (Surface: not null Surface_Type;
                                    Color  : Color_Type;
                                    Bright : Boolean);
   pragma Inline (Set_Cursor_Background);

end Ansi.Colors;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
