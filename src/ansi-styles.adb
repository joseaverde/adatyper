-------------------------------------------------------------------------------
--                                                                           --
--                       A N S I - S T Y L E S . A D B                       --
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


package body Ansi.Styles is

   ZERO: CONSTANT Natural := Character'Pos('0');
   
   ------------------------
   -- SURFACE OPERATIONS --
   ------------------------
   
   procedure Get_Style (Surface: in  not null Surface_Type;
                        Styles : out Style_Array;
                        Row    : Row_Type;
                        Col    : Col_Type) is
   begin

      Styles := Surface.Grid(Row, Col).Fmt.Style;

   exception
      when Constraint_Error =>
         raise Ansi.Exceptions.Out_Of_Bounds_Issue
         with "Index out of range!";

   end Get_Style;


   procedure Put_Style (Styles: Style_Array) is
      -- The sequence is stored in an array like:
      -- ESC[<>;<>;<>m
      -- Where <> are two bytes the first one is \0 if the Style is printed,
      -- otherwise it's '2'. The second one is the style code.
      Size    : CONSTANT Positive := Styles'Size * 3;
      Sequence: Str_Type (1 .. Size) := (others => Char_Type'Val(0));
      Pointer : Natural := 1;
   begin
      -- We add the semicolons to the string.
      for I in Natural range 1 .. Styles'Size loop
         Sequence(3*I) := ';';
      end loop;
      Sequence(Size) := 'm';

      for S in Styles'Range loop
         Sequence(Pointer + 1) := Char_Type'Val(S'Enum_Rep + ZERO);
         if not Styles(S) then
            Sequence(Pointer) := '2';
         end if;
         Pointer := Pointer + 3;
      end loop;

      Ansi.Text_IO.Put_Ansi_Sequence(ESC & Sequence);

   end Put_Style;


   procedure Put_Style (Style: Style_Type) is
   begin

      Ansi.Text_IO.Put_Ansi_Sequence(ESC &
                                     Char_Type'Val(Style'Enum_Rep + ZERO) &
                                     'm');

   end Put_Style;



   procedure Set_Style (Surface: out not null Surface_Type;
                        Styles : Style_Array;
                        Row    : Row_Type;
                        Col    : Col_Type) is
   begin
   
      if Surface.Grid(Row, Col).Fmt.Style /= Styles then
         Surface.Grid(Row, Col).Fmt.Style := Styles;
         Surface.Push(Ansi.Cursors.New_Cursor(Row, Col));
      end if;
   
   exception
      when Constraint_Error =>
         raise Ansi.Exceptions.Out_Of_Bounds_Issue
         with "Index out of range!";

   end Set_Style;


   procedure Set_Style (Surface: out not null Surface_Type;
                        Style  : Style_Type;
                        Row    : Row_Type;
                        Col    : Col_Type) is
   begin

      if not Surface.Grid(Row, Col).Fmt.Style(Style) then
         Surface.Grid(Row, Col).Fmt.Style(Style) := True;
         Surface.Push(Ansi.Cursors.New_Cursor(Row, Col));
      end if;

   exception
      when Constraint_Error =>
         raise Ansi.Exceptions.Out_Of_Bounds_Issue
         with "Index out of range!";

   end Set_Style;


   procedure Remove_Style (Surface: out not null Surface_Type;
                           Style  : Style_Type;
                           Row    : Row_Type;
                           Col    : Col_Type) is
   begin

      if Surface.Grid(Row, Col).Fmt.Style(Style) then
         Surface.Grid(Row, Col).Fmt.Style(Style) := False;
         Surface.Push(Ansi.Cursors.New_Cursor(Row, Col));
      end if;

   exception
      when Constraint_Error =>
         raise Ansi.Exceptions.Out_Of_Bounds_Issue
         with "Index out of range!";

   end Remove_Style;



   procedure Set_Style (Surface : out not null Surface_Type;
                        Styles  : Style_Array;
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
            Set_Style(Surface => Surface,
                      Styles  => Styles,
                      Row     => Row,
                      Col     => Col);
         end loop;
      end loop;

   end Set_Style;

   procedure Set_Style (Surface : out not null Surface_Type;
                        Style   : Style_Type;
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
            Set_Style(Surface => Surface,
                      Style   => Style,
                      Row     => Row,
                      Col     => Col);
         end loop;
      end loop;

   end Set_Style;


   procedure Remove_Style (Surface : out not null Surface_Type;
                           Style   : Style_Type;
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
            Set_Style(Surface => Surface,
                      Style   => Style,
                      Row     => Row,
                      Col     => Col);
         end loop;
      end loop;

   end Remove_Style;



   procedure Set_Style (Surface: out not null Surface_Type;
                        Styles : Style_Array) is
   begin

      for Row in Surface.Grid'Range(1) loop
         for Col in Surface.Grid'Range(2) loop
            Surface.Grid(Row, Col).Fmt.Style := Styles;
         end loop;
      end loop;

      Surface.Update_All := True;

   end Set_Style;


   procedure Set_Style (Surface: out not null Surface_Type;
                        Style  : Style_Type) is
   begin

      for Row in Surface.Grid'Range(1) loop
         for Col in Surface.Grid'Range(2) loop
            Surface.Grid(Row, Col).Fmt.Style(Style) := True;
         end loop;
      end loop;

      Surface.Update_All := True;

   end Set_Style;


   procedure Remove_Style (Surface: out not null Surface_Type;
                           Style  : Style_Type) is
   begin

      for Row in Surface.Grid'Range(1) loop
         for Col in Surface.Grid'Range(2) loop
            Surface.Grid(Row, Col).Fmt.Style(Style) := False;
         end loop;
      end loop;

      Surface.Update_All := True;

   end Remove_Style;




   ---------------------------------
   -- SURFACE'S CURSOR OPERATIONS --
   ---------------------------------

   procedure Get_Cursor_Style (Surface: in  not null Surface_Type;
                               Styles : out Style_Array) is
   begin

      Styles := Surface.Cursor_Fmt.Style;

   end Get_Cursor_Style;


   procedure Set_Cursor_Style (Surface: out not null Surface_Type;
                               Styles : in  Style_Array) is
   begin

      Surface.Cursor_Fmt.Style := Styles;

   end Set_Cursor_Style;


   procedure Set_Cursor_Style (Surface: out not null Surface_Type;
                               Style  : in  Style_Type) is
   begin

      Surface.Cursor_Fmt.Style(Style) := True;

   end Set_Cursor_Style;


   procedure Remove_Cursor_Style (Surface: out not null Surface_Type;
                                  Style  : in  Style_Type) is
   begin

      Surface.Cursor_Fmt.Style(Style) := False;

   end Remove_Cursor_Style;

end Ansi.Styles;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
