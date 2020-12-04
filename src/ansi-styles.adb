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

with Ansi.Compliance;
with Ansi.Cursors;
with Ansi.Exceptions;


package body Ansi.Styles is

   
   ------------------------
   -- SURFACE OPERATIONS --
   ------------------------
   
   procedure Get_Style (Surface: in  not null Surface_Type;
                        Styles : out Style_Array;
                        Row    : Row_Type;
                        Col    : Col_Type) is
   begin

      if Row > Surface.Height or Col > Surface.Width then
         raise Ansi.Exceptions.Out_Of_Bounds_Issue
         with "Index out of range!";
      end if;

      Styles := Surface.Grid(Row, Col).Fmt.Style;

   end Get_Style;


   procedure Put_Style (Styles: Style_Array) is
   begin
      
      Ansi.Compliance.Put_Style(Styles => Styles);

   end Put_Style;


   procedure Put_Style (Style: Style_Type) is
   begin

      Ansi.Compliance.Put_Style(Style => Style);

   end Put_Style;



   procedure Set_Style (Surface: in not null Surface_Type;
                        Styles : Style_Array;
                        Row    : Row_Type;
                        Col    : Col_Type) is
   begin

      if Row > Surface.Height or Col > Surface.Width then
         raise Ansi.Exceptions.Out_Of_Bounds_Issue
         with "Index out of range!";
      end if;
   
      if Surface.Grid(Row, Col).Fmt.Style /= Styles then
         Surface.Grid(Row, Col).Fmt.Style := Styles;
         Surface.Push(Ansi.Cursors.New_Cursor(Row, Col));
      end if;
   
   end Set_Style;


   procedure Set_Style (Surface: in not null Surface_Type;
                        Style  : Style_Type;
                        Row    : Row_Type;
                        Col    : Col_Type) is
   begin

      if Row > Surface.Height or Col > Surface.Width then
         raise Ansi.Exceptions.Out_Of_Bounds_Issue
         with "Index out of range!";
      end if;

      if not Surface.Grid(Row, Col).Fmt.Style(Style) then
         Surface.Grid(Row, Col).Fmt.Style(Style) := True;
         Surface.Push(Ansi.Cursors.New_Cursor(Row, Col));
      end if;

   end Set_Style;


   procedure Remove_Style (Surface: in not null Surface_Type;
                           Style  : Style_Type;
                           Row    : Row_Type;
                           Col    : Col_Type) is
   begin

      if Row > Surface.Height or Col > Surface.Width then
         raise Ansi.Exceptions.Out_Of_Bounds_Issue
         with "Index out of range!";
      end if;

      if Surface.Grid(Row, Col).Fmt.Style(Style) then
         Surface.Grid(Row, Col).Fmt.Style(Style) := False;
         Surface.Push(Ansi.Cursors.New_Cursor(Row, Col));
      end if;

   end Remove_Style;



   procedure Set_Style (Surface : in not null Surface_Type;
                        Styles  : Style_Array;
                        From_Row: Row_Type;
                        From_Col: Col_Type;
                        To_Row  : Row_Type;
                        To_Col  : Col_Type) is
   begin

      if From_Row > Surface.Height or From_Col > Surface.Width or
           To_Row > Surface.Height or   To_Col > Surface.Width then
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

   procedure Set_Style (Surface : in not null Surface_Type;
                        Style   : Style_Type;
                        From_Row: Row_Type;
                        From_Col: Col_Type;
                        To_Row  : Row_Type;
                        To_Col  : Col_Type) is
   begin

      if From_Row > Surface.Height or From_Col > Surface.Width or
           To_Row > Surface.Height or   To_Col > Surface.Width then
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


   procedure Remove_Style (Surface : in not null Surface_Type;
                           Style   : Style_Type;
                           From_Row: Row_Type;
                           From_Col: Col_Type;
                           To_Row  : Row_Type;
                           To_Col  : Col_Type) is
   begin
      
      if From_Row > Surface.Height or From_Col > Surface.Width or
           To_Row > Surface.Height or   To_Col > Surface.Width then
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



   procedure Set_Style (Surface: in not null Surface_Type;
                        Styles : Style_Array) is
   begin

      for Row in Surface.Grid'Range(1) loop
         for Col in Surface.Grid'Range(2) loop
            Surface.Grid(Row, Col).Fmt.Style := Styles;
         end loop;
      end loop;

      Surface.Update_All := True;

   end Set_Style;


   procedure Set_Style (Surface: in not null Surface_Type;
                        Style  : Style_Type) is
   begin

      for Row in Surface.Grid'Range(1) loop
         for Col in Surface.Grid'Range(2) loop
            Surface.Grid(Row, Col).Fmt.Style(Style) := True;
         end loop;
      end loop;

      Surface.Update_All := True;

   end Set_Style;


   procedure Remove_Style (Surface: in not null Surface_Type;
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


   procedure Set_Cursor_Style (Surface: in not null Surface_Type;
                               Styles : in  Style_Array) is
   begin

      Surface.Cursor_Fmt.Style := Styles;

   end Set_Cursor_Style;


   procedure Set_Cursor_Style (Surface: in not null Surface_Type;
                               Style  : in  Style_Type) is
   begin

      Surface.Cursor_Fmt.Style(Style) := True;

   end Set_Cursor_Style;


   procedure Remove_Cursor_Style (Surface: in not null Surface_Type;
                                  Style  : in  Style_Type) is
   begin

      Surface.Cursor_Fmt.Style(Style) := False;

   end Remove_Cursor_Style;

end Ansi.Styles;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
