-------------------------------------------------------------------------------
--                                                                           --
--                     A N S I - S U R F A C E S . A D B                     --
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
with Ansi.Colors;
with Ansi.Exceptions;
with Ansi.Styles;
with Ansi.Text_IO;
with Ada.Text_IO;


package body Ansi.Surfaces is
   

   function Create (Height: Row_Type;
                    Width : Col_Type)
                    return Surface_Type is
   begin

      return New_Surface: Surface_Type do
         New_Surface := new Surface_Record(Height, Width);
         New_Surface.Cursor := new Ansi.Cursors.Cursor_Type;
         New_Surface.Cursor.Set_Position(1, 1, False);
      end return;

   end Create;



   function Get_Cursor (Surface: Surface_Type)
                        return Cursor_Type is
   begin

      return Surface.Cursor;

   end Get_Cursor;
   


   procedure Put (Item   : Str_Type;
                  Surface: Surface_Type := null) is
   begin

      for Char of Item loop
         Put(Item    => Char,
             Surface => Surface);
      end loop;

   exception
      when Ansi.Exceptions.Out_Of_Bounds_Issue =>
         raise Ansi.Exceptions.Out_Of_Bounds_Issue
         with "String went out of bounds!";
   end Put;
   
   
   
   procedure Put (Item   : Char_Type;
                  Surface: Surface_Type := null) is
      Surf: Surface_Type := (if Surface = null then
                              Main_Surface
                             else
                              Surface);
   begin
      
      if Surf.Cursor.Get_Col > Col_Type(Surf.Width) then
         raise Ansi.Exceptions.Out_Of_Bounds_Issue
         with "Character went out of bounds!";
      end if;

      Surf.Grid(Surf.Cursor.Get_Row, Surf.Cursor.Get_Col).Char := Item;
      Surf.Grid(Surf.Cursor.Get_Row, Surf.Cursor.Get_Col).Fmt  := Surf.
                                                                  Cursor_Fmt;
      Surf.Push(Surf.Cursor);
      Surf.Cursor.Move_Right(1);
      
   end Put;
   
   
   -- XXX: Bug found, when printing the surface the last row is somehow printed
   -- onto the first row at least when printing into the first row. There is no
   -- problem with columns.
   --
   -- When printing a 5x5 there is no problem, though.
   --
   -- TESTS: 1) Test 9x9, 8x8, and over 10x10, 11x11. If it only happens with
   --           two digit numbers, then it's the problem of the function that
   --           converts the numbers into strings. (Even thought the number of
   --           columns seems right)
   --        2) Test it without moving a line down.
   --
   -- POSIBLE CAUSES: Maybe is the fault of the terminal emulator or the ANSI
   --                 escape sequences. In that case, (knowing that it only
   --                 happens in the first row) would be to either increase the
   --                 number of rows of the matrix by one.
   --
   -- POSIBLE SOLUTIONS: Instead of using numbers use the range of the grid
   --                    (matrix) instead.
   --
   procedure Put (Surface: Surface_Type := null;
                  Row    : Row_Type := 1;
                  Col    : Col_Type := 1) is
      Surf: Surface_Type := (if Surface = null then
                              Main_Surface
                             else
                              Surface);
      Item: Element;
      
      File: Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create(Name => "Put.log",
                         File => File,
                         Mode => Ada.Text_IO.Out_File);
      Main_Cursor.Set_Position(Row, Col);
      -- delay 1.0;
      for Y in Row_Type range 1 .. Surf.Height loop
         for X in Col_Type range 1 .. Surf.Width loop
            Item := Surf.Grid(Y, X);
            -- TODO: Optimize it, checking if the last colour used was the same
            -- as the colour now and if so change the colour. Even though this
            -- function won't be used because the layers will give a more
            -- optimized version, but it's good to have it for debugging in
            -- early stages.
            Ansi.Colors.Put_Foreground(Color  => Item.Fmt.Fg_Color,
                                       Bright => Item.Fmt.Fg_Bright);
            Ansi.Colors.Put_Background(Color  => Item.Fmt.Bg_Color,
                                       Bright => Item.Fmt.Bg_Bright);
            --Ansi.Styles.Put_Styles(Styles => Item.Fmt.Styles);
           -- Ada.Text_IO.Put(Char_Type'Pos(Item.Char)'Image);
            Ada.Text_IO.Put(File => File,
                            Item => Character'Val(Char_Type'Pos(Item.Char)));
            Ansi.Text_IO.Put(Item.Char);
            -- delay 0.01;
         end loop;
         Main_Cursor.Set_Position(Row + Y, Col);
         --Main_Cursor.Move_Down;
         --Main_Cursor.Set_Col(Col);
         Ada.Text_IO.New_Line(File => File);
         Ada.Text_IO.Put_Line(File => File,
                              Item => "ROW" & Main_Cursor.Get_Row'Image & " :: " &
                                      "COL" & Main_Cursor.Get_Col'Image & " :: " &
                                      "Y "  & Y'Image);
      end loop;
      Ada.Text_IO.Close(File);
   end Put;


end Ansi.Surfaces;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
