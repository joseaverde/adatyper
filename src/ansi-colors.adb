-------------------------------------------------------------------------------
--                                                                           --
--                       A N S I - C O L O R S . A D B                       --
--                                                                           --
--                              A D A T Y P E R                              --
--                                                                           --
--                                  B O D Y                                  --
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

with Ansi.Cursors;
with Ansi.Exceptions;
with Ansi.Text_IO;


package body Ansi.Colors is

   ZERO: CONSTANT Natural := Character'Pos('0');
   -- TODO: Get Constraint_Error and raise Out_Bound_Error;

   ------------------------
   -- SURFACE OPERATIONS --
   ------------------------

   procedure Get_Foreground (Surface: in  not null Surface_Type;
                             Color  : out Color_Type;
                             Bright : out Boolean;
                             Row    :     Row_Type;
                             Col    :     Col_Type) is
   begin
      
      Color  := Surface.Grid(Row, Col).Fmt.Fg_Color;
      Bright := Surface.Grid(Row, Col).Fmt.Fg_Bright;

   exception
      when Constraint_Error =>
         raise Ansi.Exceptions.Out_Of_Bounds_Issue
         with "Index out of range!";

   end Get_Foreground;


   procedure Get_Background (Surface: in  not null Surface_Type;
                             Color  : out Color_Type;
                             Bright : out Boolean;
                             Row    :     Row_Type;
                             Col    :     Col_Type) is
   begin

      Color  := Surface.Grid(Row, Col).Fmt.Bg_Color;
      Bright := Surface.Grid(Row, Col).Fmt.Bg_Bright;

   exception
      when Constraint_Error =>
         raise Ansi.Exceptions.Out_Of_Bounds_Issue
         with "Index out of range!";

   end Get_Background;



   procedure Put_Foreground (Color : Color_Type;
                             Bright: Boolean) is
   begin

      Ansi.Text_IO.Put_Ansi_Sequence(ESC &
                                     (if Bright then
                                       "9"
                                      else
                                       "3") &
                                     Char_Type'Val(ZERO + Color'Enum_Rep) &
                                     "m");

   end Put_Foreground;


   procedure Put_Background (Color : Color_Type;
                             Bright: Boolean) is
   begin

      Ansi.Text_IO.Put_Ansi_Sequence(ESC &
                                     (if Bright then
                                       "10"
                                      else
                                       "4") &
                                     Char_Type'Val(ZERO + Color'Enum_Rep) &
                                     "m");

   end Put_Background;



   procedure Set_Foreground (Surface: out not null Surface_Type;
                             Color  : Color_Type;
                             Bright : Boolean;
                             Row    : Row_Type;
                             Col    : Col_Type) is
   begin
      
      -- We check whether we are overwriting the colour.
      if Surface.Grid(Row, Col).Fmt.Fg_Color  /= Color  or
         Surface.Grid(Row, Col).Fmt.Fg_Bright /= Bright
      then
         -- If so we change it and push it to the tail.
         Surface.Grid(Row, Col).Fmt.Fg_Color  := Color;
         Surface.Grid(Row, Col).Fmt.Fg_Bright := Bright;
         Surface.Push(Ansi.Cursors.New_Cursor(Row, Col));
      end if;

   exception
      when Constraint_Error =>
         raise Ansi.Exceptions.Out_Of_Bounds_Issue
         with "Index out of range!";

   end Set_Foreground;


   procedure Set_Background (Surface: out not null Surface_Type;
                             Color  : Color_Type;
                             Bright : Boolean;
                             Row    : Row_Type;
                             Col    : Col_Type) is
   begin
      
      -- We check if we are overwriting the colour.
      if Surface.Grid(Row, Col).Fmt.Bg_Color  /= Color or
         Surface.Grid(Row, Col).Fmt.Bg_Bright /= Bright
      then
         -- If so we change it and push it to the tail.
         Surface.Grid(Row, Col).Fmt.Bg_Color  := Color;
         Surface.Grid(Row, Col).Fmt.Bg_Bright := Bright;
         Surface.Push(Ansi.Cursors.New_Cursor(Row, Col));
      end if;

   exception
      when Constraint_Error =>
         raise Ansi.Exceptions.Out_Of_Bounds_Issue
         with "Index out of range!";

   end Set_Background;



   procedure Set_Foreground (Surface : out not null Surface_Type;
                             Color   : Color_Type;
                             Bright  : Boolean;
                             From_Row: Row_Type;
                             From_Col: Col_Type;
                             To_Row  : Row_Type;
                             To_Col  : Col_Type) is
   begin

      if To_Row > Surface.Height or To_Col > Surface.Width then
         raise Ansi.Exceptions.Out_Of_Bounds_Issue
         with "Range out of bounds!";
      end if;

      for Row in Row_Type range From_Row .. To_Row loop
         for Col in Col_Type range From_Col .. To_Col loop
            Set_Foreground(Surface => Surface,
                           Color   => Color,
                           Bright  => Bright,
                           Row     => Row,
                           Col     => Col);
         end loop;
      end loop;

   end Set_Foreground;


   procedure Set_Background (Surface : out not null Surface_Type;
                             Color   : Color_Type;
                             Bright  : Boolean;
                             From_Row: Row_Type;
                             From_Col: Col_Type;
                             To_Row  : Row_Type;
                             To_Col  : Col_Type) is
   begin

      if To_Row > Surface.Height or To_Col > Surface.Width then
         raise Ansi.Exceptions.Out_Of_Bounds_Issue
         with "Range out of bounds!";
      end if;

      for Row in Row_Type range From_Row .. To_Row loop
         for Col in Col_Type range From_Col .. To_Col loop
            Set_Background(Surface => Surface,
                           Color   => Color,
                           Bright  => Bright,
                           Row     => Row,
                           Col     => Col);
         end loop;
      end loop;

   end Set_Background;


   procedure Set_Foreground (Surface: out not null Surface_Type;
                             Color  : Color_Type;
                             Bright : Boolean) is
   begin

      for Row in Surface.Grid'Range(1) loop
         for Col in Surface.Grid'Range(2) loop
            Surface.Grid(Row, Col).Fmt.Fg_Color  := Color;
            Surface.Grid(Row, Col).Fmt.Fg_Bright := Bright;
         end loop;
      end loop;

      Surface.Update_All := True;

   end Set_Foreground;
   

   procedure Set_Background (Surface: out not null Surface_Type;
                             Color  : Color_Type;
                             Bright : Boolean) is
   begin

      for Row in Surface.Grid'Range(1) loop
         for Col in Surface.Grid'Range(2) loop
            Surface.Grid(Row, Col).Fmt.Bg_Color  := Color;
            Surface.Grid(Row, Col).Fmt.Bg_Bright := Bright;
         end loop;
      end loop;

      Surface.Update_All := True;

   end Set_Background;




   ---------------------------------
   -- SURFACE'S CURSOR OPERATIONS --
   ---------------------------------

   procedure Get_Cursor_Foreground (Surface: in  not null Surface_Type;
                                    Color  : out Color_Type;
                                    Bright : out Boolean) is
   begin

      Color  := Surface.Cursor_Fmt.Fg_Color;
      Bright := Surface.Cursor_Fmt.Fg_Bright;

   end Get_Cursor_Foreground;


   procedure Get_Cursor_Background (Surface: in  not null Surface_Type;
                                    Color  : out Color_Type;
                                    Bright : out Boolean) is
   begin

      Color  := Surface.Cursor_Fmt.Bg_Color;
      Bright := Surface.Cursor_Fmt.Bg_Bright;

   end Get_Cursor_Background;



   procedure Set_Cursor_Foreground (Surface: out not null Surface_Type;
                                    Color  : Color_Type;
                                    Bright : Boolean) is
   begin

      Surface.Cursor_Fmt.Fg_Color  := Color;
      Surface.Cursor_Fmt.Fg_Bright := Bright;

   end Set_Cursor_Foreground;


   procedure Set_Cursor_Background (Surface: out not null Surface_Type;
                                    Color  : Color_Type;
                                    Bright : Boolean) is
   begin

      Surface.Cursor_Fmt.Bg_Color  := Color;
      Surface.Cursor_Fmt.Bg_Bright := Bright;

   end Set_Cursor_Background;

end Ansi.Colors;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
