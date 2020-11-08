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


with Ada .Unchecked_Deallocation;
with Ansi.Cursors;
with Ansi.Colors;
with Ansi.Exceptions;
with Ansi.Styles;
with Ansi.Text_IO;
-- with Ada.Text_IO; DEBUGING


package body Ansi.Surfaces is


   ----------------------------
   -- FUNCTIONS FOR SURFACES --
   ----------------------------

   function Copy (Surface: Surface_Type)
                  return Surface_Type is
      Copied_Surface: Surface_Type := Create(Height => Surface.Height,
                                             Width  => Surface.Width);
   begin

      Copied_Surface.Grid := Surface.Grid;
      Copied_Surface.Update_All := True;
      Copied_Surface.Cursor.Set_Position(Surface.Cursor, False);
      Copied_Surface.Cursor_Fmt := Surface.Cursor_Fmt;
      Copied_Surface.Protect_It := Surface.Protect_It;

      return Copied_Surface;

   end Copy;
   

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


   procedure Free (Surface: in out Surface_Type) is
      procedure Deallocate is new Ada.Unchecked_Deallocation(Surface_Record,
                                                             Surface_Type);
      Temp_Queue: Operation;
      Next_Queue: Operation := Surface.Head;
   begin
      
      -- We first clean the queues and their cursors.
      if Next_Queue /= null then
         while Next_Queue /= null loop
            Temp_Queue := Next_Queue;
            Next_Queue := Next_Queue.Next;
            Ansi.Cursors.Free(Temp_Queue.Cursor);
            Free(Temp_Queue);
         end loop;
      end if;

      -- Finally we deallocate the cursor and the surface.
      Ansi.Cursors.Free(Surface.Cursor);
      Deallocate(Surface);

   end Free;



   function Get_Cursor (Surface: Surface_Type)
                        return Cursor_Type is
   begin

      return Surface.Cursor;

   end Get_Cursor;


   procedure Paste (Over   : Surface_Type;
                    Surface: Surface_Type;
                    Row    : Row_Type;
                    Col    : Col_Type) is
      To_Row: Row_Type := (if Over.Height - Row < Surface.Height then
                              Over.Height
                           else
                              Row + Surface.Height);
      To_Col: Col_Type := (if Over.Width - Col < Surface.Width then
                              Over.Width
                           else
                              Col + Surface.Width);
   begin

      if Row > Over.Height or Col > Over.Width then
         raise Ansi.Exceptions.Out_Of_Bounds_Issue
         with "Trying to paste out of range!";
      end if;
      
      for R in Row_Type range Row .. To_Row loop
         for C in Col_Type range Col .. To_Col loop
            Over.Grid(R, C) := Surface.Grid(R - Row, C - Col);
            Over.Push(Ansi.Cursors.New_Cursor(R, C));
         end loop;
      end loop;

   end Paste;


   procedure Put (Item   : Str_Type;
                  Surface: Surface_Type := null;
                  Feed   : Boolean      := False) is
   begin

      for Char of Item loop
         Put(Item    => Char,
             Surface => Surface,
             Feed    => Feed);
      end loop;

   exception
      when Ansi.Exceptions.Out_Of_Bounds_Issue =>
         raise Ansi.Exceptions.Out_Of_Bounds_Issue
         with "String went out of bounds!";
   end Put;
   
   
   
   procedure Put (Item   : Char_Type;
                  Surface: Surface_Type := null;
                  Feed   : Boolean      := False) is
      Surf: Surface_Type := (if Surface = null then
                              Main_Surface
                             else
                              Surface);
   begin
      
      if Surf.Cursor.Get_Col > Col_Type(Surf.Width) then
         if Feed then
            Surf.Cursor.Set_Position(Surf.Cursor.Get_Row + 1, 1, False);
         else
            raise Ansi.Exceptions.Out_Of_Bounds_Issue
            with "Character went out of bounds!";
         end if;
      end if;

      Surf.Grid(Surf.Cursor.Get_Row, Surf.Cursor.Get_Col).Char := Item;
      Surf.Grid(Surf.Cursor.Get_Row, Surf.Cursor.Get_Col).Fmt  := Surf.
                                                                  Cursor_Fmt;
      Surf.Push(Surf.Cursor);
      Surf.Cursor.Move_Right(1);
      
   end Put;
   
   
   -- FIXED!!!
   -- XXX: Bug found, when printing the surface the last row is somehow printed
   -- onto the first row at least when printing into the first row. There is no
   -- problem with columns.
   --
   -- When printing a 5x5 there is no problem, though.
   --
   -- TESTS: 1) Test 9x9, 8x8, and over 10x10, 11x11. If it only happens with
   --           two digit numbers, then it's the problem of the function that
   --           converts the numbers into strings. (Even thought the number of
   --           columns seems right) [DONE]
   --
   --           REPLICATION FOUND: The problem happens when the number of rows
   --           is over 10, which then turns into 9 (independently from the
   --           intended number of rows)
   --           It seems to be caused from the To_String | the range of the
   --           function | the ANSI itself.
   --
   --
   --        2) Test it without moving a line down. [DONE]
   --           
   --           When the line doesn't move down, it's all written in the same
   --           line, so it's seems it's not the problem of ANSI.
   --
   --
   --        3) Test to write the escape sequences into a file or to write
   --           them without the escape. [DONE]
   --
   --           It's unreadable.
   --
   -- POSIBLE CAUSES: Maybe is the fault of the terminal emulator or the ANSI
   --                 escape sequences. In that case, (knowing that it only
   --                 happens in the first row) would be to either increase the
   --                 number of rows of the matrix by one.
   --
   -- POSIBLE SOLUTIONS: Instead of using numbers use the range of the grid
   --                    (matrix) instead. [DONE]
   --
   --                    The problem still occurs.
   --
   -- BUG CAUSE FOUND: After tinkering with Python3 and doing the same, it
   -- doesn't seem to be a problem of ANSI or the program itself. The bug is
   -- found in the Ansi.Cursors package To_String function, that doesn't
   -- return the first cypher of the number. That's why 10 overflows to 0,
   -- 11 to 1 and 12 to 2.
   --
   procedure Put (Surface: Surface_Type := null;
                  Row    : Row_Type := 1;
                  Col    : Col_Type := 1) is
      Surf: Surface_Type := (if Surface = null then
                              Main_Surface
                             else
                              Surface);
      Item: Element;
   begin

      Main_Cursor.Set_Position(Row, Col);
      for Y in Surf.Grid'Range(1) loop
         for X in Surf.Grid'Range(2) loop
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
            Ansi.Text_IO.Put(Item.Char);
            -- delay 0.01; A good effect
         end loop;
         -- This one is faster because the other two will call the To_String
         -- function three times.
         Main_Cursor.Set_Position(Row + Y, Col);
         -- Main_Cursor.Move_Down;
         -- Main_Cursor.Set_Col(Col);
      end loop;

   end Put;


   procedure Resize (Surface   : in out not null Surface_Type;
                     Rows_Up   : Integer := 0;
                     Rows_Down : Integer := 0;
                     Cols_Left : Integer := 0;
                     Cols_Right: Integer := 0) is
      Resized_Surface: Surface_Type :=
                 new Surface_Record(Height => Surface.Height +
                                             Row_Type(Rows_Up + Rows_Down),
                                    Width  => Surface.Width +
                                             Col_Type(Cols_Right + Cols_Left));

      -- We first declare the bounds of the old surface that will be copied.
      Surface_Up  : Row_Type := (if Rows_Up < 0 then
                                    Row_Type(abs Rows_Up)
                                 else
                                    Surface.Grid'First(1));
      Surface_Down: Row_Type := (if Rows_Down < 0 then
                                    Row_Type(abs Rows_Down)
                                 else
                                    Surface.Grid'Last(1));

      Surface_Left : Col_Type := (if Cols_Left < 0 then
                                    Col_Type(abs Cols_Left)
                                  else
                                    Surface.Grid'First(2));
      Surface_Right: Col_Type := (if Cols_RIght < 0 then
                                    Col_Type(abs Cols_Right)
                                  else
                                    Surface.Grid'Last(2));

   begin

      -- We first copy the grid (the matrix) from the old one to the new one.
      for Row in Row_Type range Surface_Up .. Surface_Down loop
         for Col in Col_Type range Surface_Left .. Surface_Right loop
            -- Now we start writing in the new surface.
            Resized_Surface.Grid(
                  Row_Type(Integer(Surface_Up) + Rows_Up),
                  Col_Type(Integer(Surface_Left) + Cols_Left))
                     := Surface.Grid(Row, Col);
         end loop;
      end loop;
      
      -- We copy the rest of the record. Except the stack, because the layer
      -- will be completely freed and it will be completely updated.
      Resized_Surface.Update_All := True;
      Resized_Surface.Cursor := Ansi.Cursors.New_Cursor
                                          (Row => Surface.Cursor.Get_Row,
                                           Col => Surface.Cursor.Get_Col);
      Resized_Surface.Cursor_Fmt := Surface.Cursor_Fmt;
      Resized_Surface.Protect_It := Surface.Protect_It;
      
      -- We set the position where the original image was.
      Resized_Surface.Row := Row_Type(Integer(Surface.Row) + Rows_Up);
      Resized_Surface.Col := Col_Type(Integer(Surface.Col) + Cols_Left);

      -- Finally we free it and change the old one.
      Free(Surface);
      Surface := Resized_Surface;

   exception
      when Constraint_Error =>
         -- TODO: Maybe free the old surface.
         Free(Resized_Surface);
         raise Ansi.Exceptions.Windows_Size_Issue
         with "Couldn't create a new surface";

   end Resize;


   ---------------------
   -- LAYER FUNCTIONS --
   ---------------------
   
   procedure Add (Layerer: Layerer_Type;
                  Layer  : Surface_Type) is
   begin
      null; -- TODO
   end Add;


   procedure Remove (Layerer: Layerer_Type;
                     Layer  : Surface_Type) is
   begin
      null; -- TODO
   end Remove;

   
   procedure Update (Layerer: Layerer_Type) is
   begin
      null; -- TODO
   end Update;

   
   procedure Hide (Layerer: in out Layerer_Type;
                   Layer  : Surface_Type) is
   begin
      null; -- TODO
   end Hide;


   procedure Show (Layerer: in out Layerer_Type;
                   Layer  : Surface_Type) is
   begin
      null; -- TODO
   end Show;



   function Contains_Layer (Layerer: in Layerer_Type;
                            Layer  : Surface_Type)
                            return Boolean is
   begin
      return False; -- TODO
   end Contains_Layer;



   function Get_Layer_Number (Layerer: in Layerer_Type)
                              return Natural is
   begin

      return Layerer.Layers'Length;

   end Get_Layer_Number;


   function Get_Position (Layerer: in Layerer_Type;
                          Layer  : Surface_Type)
                          return Positive is
   begin
      return 1; -- TODO
   end Get_Position;


end Ansi.Surfaces;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
