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
with Debug; use Debug; -- DEBUGING
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


   function Create (Grid: Str_Type_Array)
                    return Surface_Type is
   begin

      return New_Surface: Surface_Type do
         New_Surface := Create(Grid'Length(1), Grid'Length(2));
         New_Surface.Update_All := True;
         for Row in New_Surface.Grid'Range(1) loop
            for Col in New_Surface.Grid'Range(2) loop
               New_Surface.Grid(Row, Col).Char := Grid(Positive(Row),
                                                       Positive(Col));
            end loop;
         end loop;
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
            Ansi.Styles.Put_Style(Styles => Item.Fmt.Style);
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
   
   procedure Add (Layerer : in out Layerer_Type;
                  Layer   : Surface_Type;
                  Position: Natural := 0) is
      Old_Layerer: Surface_Array := Layerer.Layers.all;
      Old_Visible: Boolean_Array := Layerer.Visible.all;
      Old_Size : Natural := Layerer.Size;
   begin

      -- We check if it is already there.
      if Layerer / Layer then
         raise Ansi.Exceptions.Already_Inside_Layerer_Issue
         with "The surface is already inside the layerer!";
      end if;
      
      -- We free the old arrays.
      Free(Layerer.Layers);
      Free(Layerer.Visible);

      -- We allocate new ones.
      Layerer.Layers  := new Surface_Array(1 .. Old_Size + 1);
      Layerer.Visible := new Boolean_Array(1 .. Old_Size + 1);
      Layerer.Size    := Old_Size + 1;
      
      if Position = 0 then
         -- We update the layers.
         Layerer.Layers(1 .. Old_Size) := Old_Layerer;
         Layerer.Layers(Layerer.Layers'Last) := Layer;

         -- We update the visibility.
         Layerer.Visible(1 .. Old_Size) := Old_Visible;
         Layerer.Visible(Layerer.Visible'Last) := True;
      else
         -- We update the layers.
         Layerer.Layers(1 .. Position - 1) := Old_Layerer(Old_Layerer'First ..
                                                          Old_Layerer'First +
                                                            Position - 1);
         Layerer.Layers(Position) := Layer;
         Layerer.Layers(Position + 1 .. Layerer.Layers'Last) :=
            Old_Layerer(Old_Layerer'First + Position + 1 .. Old_Layerer'Last);

         -- We update the visibility.
         Layerer.Visible(1 .. Position - 1) := Old_Visible(Old_Visible'First ..
                                                           Old_Visible'First +
                                                            Position - 1);
         Layerer.Visible(Position) := True;
         Layerer.Visible(Position + 1 .. Layerer.Visible'Last) :=
            Old_Visible(Old_Visible'First + Position + 1 .. Old_Visible'Last);
      end if;

   end Add;


   procedure Remove (Layerer: in out Layerer_Type;
                     Layer  : Surface_Type) is
   begin

      for L in Layerer.Layers'Range loop
         if Layerer.Layers(L) = Layer then
            Shrink_Layerer:
               declare
                  Old_Size : Natural := Layerer.Layers'Length;
                  New_Layer: Layer_Array := new Surface_Array(1 .. Old_Size-1);
               begin
                  New_Layer(1 .. L - 1) := Layerer.Layers(1 .. L - 1);
                  New_Layer(L .. New_Layer'Last) := Layerer.Layers(L + 1 ..
                                                      Layerer.Layers'Last);
                  Free(Layerer.Layers);
                  Layerer.Layers := New_Layer;
               end Shrink_Layerer;
            return;
         end if;
      end loop;

      raise Ansi.Exceptions.Unknown_Layer_Issue
      with "The given layer can't be removed because it isn't in the Layerer";

   end Remove;


   procedure Remove (Layerer : in out Layerer_Type;
                     Position: Positive) is
      New_Layer: Layer_Array := new Surface_Array(1..Layerer.Layers'Length-1);
   begin
      New_Layer(1 .. Position - 1) := Layerer.Layers(1 .. Position - 1);
      New_Layer(Position .. New_Layer'Last) := Layerer.Layers(Position + 1 ..
                                                      Layerer.Layers'Last);
      Free(Layerer.Layers);
      Layerer.Layers := New_Layer;
   end Remove;



   function Is_In (Layerer: Layerer_Type;
                   Layer  : Surface_Type)
                   return Boolean is
   begin

      for L in Layerer.Layers'Range loop
         if Layerer.Layers(L) = Layer then
            return True;
         end if;
      end loop;

      return False;

   end Is_In;


   
   procedure Update (Layerer: Layerer_Type) is
      Layer : Surface_Type;
      Hidden: Element := Element'(Fmt  => Format'(Fg_Color  => White,
                                                  Fg_Bright => False,
                                                  Bg_Color  => Black,
                                                  Bg_Bright => False,
                                                  Style     =>(others=>False)),
                                  Char => ' ');
   begin
      -- We will start to go through all the Layers, checking which ones have
      -- been updated and pass those changes to the main layer. First we clean
      -- the space where the hidden layers are found.
      
      Hidden_Loop:
      for L in Positive range 1 .. Layerer.Size loop
--BREAKPOINT("HIDDEN_LOOP::POSITION" & L'IMAGE & " /" & LAYERER.SIZE'IMAGE);
         Layer := Layerer.Layers(L);
         if not Layerer.Visible(L) and Layer.Update_All then
            -- We clean the surfaces where the layers would be.
            Cleaner:
               declare
                  -- We declare the bounds of the layer.
                  Lower_Row: Row_Type;
                  Upper_Row: Row_Type;
                  Lower_Col: Col_Type;
                  Upper_Col: Col_Type;
               begin
                  -- We first check the layer is out of the screen.
                  if Layer.Row > Height or Layer.Col > Width then
                     goto Exit_Cleaner;
                  end if;

                  Lower_Row := Layer.Row;
                  Lower_Col := Layer.Col;
                  Upper_Row := (if Layer.Row + Layer.Height > Height then
                                       Height
                                else
                                       Layer.Row + Layer.Height
                                );
                  Upper_Col := (if Layer.Col + Layer.Width > Width then
                                       Width
                                else
                                       Layer.Col + Layer.Width
                                );

                  for Row in Row_Type range Lower_Row .. Upper_Row loop
                     for Col in Col_Type range Lower_Col .. Upper_Col loop
                        if Main_Surface.Grid(Row, Col) /= Hidden then
                           Main_Surface.Grid(Row, Col) := Hidden;
                           Main_Surface.Push(Ansi.Cursors.New_Cursor(Row,Col));
                        end if;
                     end loop;
                  end loop;

                  <<Exit_Cleaner>>
               end Cleaner;
            -- We don't have to remove the queue because it would take too much
            -- time. It's better to remove it once it's shown again.
         end if;
      end loop Hidden_Loop;

      Layerer_Loop:
      for L in Positive range 1 .. Layerer.Size loop
--BREAKPOINT("LAYERER_LOOP::POSITION" & L'IMAGE & " /" & LAYERER.SIZE'IMAGE);
         -- We check if it has a stack or we have to update everything.
         Layer := Layerer.Layers(L);
         -- If it's hidden we don't do anything.
         if not Layerer.Visible(L) then
            goto Continue_Layerer_Loop;
         end if;
         if Layer.Update_All then
            -- We update everything from the Main_Layer.
            Update_All_Main_Layer:
               declare
                  -- We declare the bounds of the layer.
                  Lower_Row: Row_Type;
                  Upper_Row: Row_Type;
                  Lower_Col: Col_Type;
                  Upper_Col: Col_Type;

                  -- We declare some access type for the stack.
                  Next_Node: Operation := Layer.Head;
                  Temp_Node: Operation;
               begin
                  -- We first check the layer isn't out of the screen.
                  if Layer.Row > Height or Layer.Col > Width then
                     goto Exit_Update_All_Main_Layer;
                  end if;

                  -- Then we start to assing the ranges.
                  Lower_Row := Layer.Row;
                  Lower_Col := Layer.Col;
                  
                  Upper_Row := (if Layer.Row + Layer.Height > Height then
                                       Height
                                else
                                       Layer.Row + Layer.Height -  1 
                               );
                  Upper_Col := (if Layer.Col + Layer.Width > Width then
                                       Width
                                else
                                       Layer.Col + Layer.Width - 1
                               );
--BREAKPOINT("BOUNDS SET", 4);
--BREAKPOINT("LOWER_ROW =" & LOWER_ROW'IMAGE, 7);
--BREAKPOINT("UPPER_ROW =" & UPPER_ROW'IMAGE, 7);
--BREAKPOINT("LOWER_COL =" & LOWER_COL'IMAGE, 7);
--BREAKPOINT("UPPER_COL =" & UPPER_COL'IMAGE, 7);
--BREAKPOINT("WIDTH  =" &  MAIN_SURFACE.WIDTH'IMAGE, 7);
--BREAKPOINT("HEIGHT =" & MAIN_SURFACE.HEIGHT'IMAGE, 7);
--BREAKPOINT("LAYER.ROW =" & LAYER.ROW'IMAGE, 7);
--BREAKPOINT("LAYER.COL =" & LAYER.COL'IMAGE, 7);
                  -- We apply the changes to the Main_Surface.
                  for Row in Row_Type range Lower_Row .. Upper_Row loop
                     for Col in Col_Type range Lower_Col .. Upper_Col loop
--BREAKPOINT("SET ("&ROW'IMAGE&COL'IMAGE&" )", 10);
                        if Layer.Grid(1+Row-Layer.Row, 1+Col-Layer.Col) /=
                           Main_Surface.Grid(Row, Col)
                        then
                           Main_Surface.Grid(Row, Col) := Layer.Grid
                                                            (1+Row-Layer.Row,
                                                             1+Col-Layer.Col);
                           -- We push another element to the Main_Surface stack
                           Main_Surface.Push(Ansi.Cursors.New_Cursor(Row, Col));
--BREAKPOINT("PUSHED", 10);
                        end if;
                     end loop;
                  end loop;
--BREAKPOINT("GRID WRITTEN", 4);

                  -- We finally free the stack.
                  Layer.Update_All := False;
                  while Next_Node /= null loop
                     Temp_Node := Next_Node;
                     Next_Node := Next_Node.Next;
                     Ansi.Cursors.Free(Temp_Node.Cursor);
                     Free(Temp_Node);
                  end loop;
                  Layer.Head := null;
                  Layer.Tail := null;
--BREAKPOINT("QUEUE FREED", 4);
                  
                  <<Exit_Update_All_Main_Layer>>
               end Update_All_Main_Layer;

         elsif Layer.Head /= null then
            -- Now we have to update the other kind of updated layer.
            -- The one that hasn't be told to be updated completely, but the
            -- queue.
            Update_Queue_Main_Layer:
               declare
                  -- We read the queue.
                  Next_Node: Operation := Layer.Head;
                  Temp_Node: Operation;
                  Row: Row_Type;
                  Col: Col_Type;
               begin

                  while Next_Node /= null loop
                     Row := Next_Node.Cursor.Get_Row;
                     Col := Next_Node.Cursor.Get_Col;

                     -- We check it isn't out of bounds.
                     if Row + Layer.Row > Height or Col + Layer.Col > Width
                     then
                        Ansi.Cursors.Free(Next_Node.Cursor);
                        goto Position_Out_Of_Range;
                     end if;

                     -- We check if it's different from the one on the main
                     -- surface, if so, we don't update it.
                     if Main_Surface.Grid(Row + Layer.Row, Col + Layer.Col) /=
                        Layer.Grid(Row, Col)
                     then
                        Main_Surface.Grid(Row + Layer.Row, Col + Layer.Col) :=
                                             Layer.Grid(Row, Col);
                        -- Also, we don't have to allocate a new cursor, we can
                        -- use the one we already have.
                        Main_Surface.Push(Next_Node.Cursor);
                     else
                        -- We update it.
                        Ansi.Cursors.Free(Next_Node.Cursor);
                     end if;
                     <<Position_Out_Of_Range>>
                     -- We free it and continue.
                     Temp_Node := Next_Node;
                     Next_Node := Next_Node.Next;
                     Free(Next_Node);
                  end loop;
                  Layer.Head := null;
                  Layer.Tail := null;

               end Update_Queue_Main_Layer;
         end if;

         <<Continue_Layerer_Loop>>
      end loop Layerer_Loop;

   end Update;

   
   procedure Hide (Layerer: in out Layerer_Type;
                   Layer  : Surface_Type) is
   begin

      for I in Positive range 1 .. Layerer.Size loop
         if Layerer.Layers(I) = Layer then
            if Layerer.Visible(I) then
               Layer.Update_All := True;
               Layerer.Visible(I) := False;
            end if;
            return;
         end if;
      end loop;

      raise Ansi.Exceptions.Unknown_Layer_Issue
      with "The layer to be hidden isn't in the layerer!";

   end Hide;


   procedure Hide (Layerer : in out Layerer_Type;
                   Position: Positive) is
   begin

      if Position > Layerer.Size then
         raise Ansi.Exceptions.Out_Of_Bounds_Issue
         with "The layer's position is out of the layerer's bounds!";
      end if;
      
      if Layerer.Visible(Position) Then
         Layerer.Layers(Position).Update_All := True;
         Layerer.Visible(Position) := False;
      end if;

   end Hide;


   procedure Show (Layerer: in out Layerer_Type;
                   Layer  : Surface_Type) is
   begin

      for I in Positive range 1 .. Layerer.Size loop
         if Layerer.Layers(I) = Layer then
            if not Layerer.Visible(I) then
               Layer.Update_All := True;
               Layerer.Visible(I) := True;
            end if;
            return;
         end if;
      end loop;

      raise Ansi.Exceptions.Unknown_Layer_Issue
      with "The layer to be hidden isn't in the layerer!";

   end Show;


   procedure Show (Layerer : in out Layerer_Type;
                   Position: Positive) is
   begin
      
      if Position > Layerer.Size then
         raise Ansi.Exceptions.Out_Of_Bounds_Issue
         with "The layer's position is out of the layerer's bounds!";
      end if;

      if not Layerer.Visible(Position) then
         Layerer.Layers(Position).Update_All := True;
         Layerer.Visible(Position) := True;
      end if;

   end Show;



   function Get_Layer_Number (Layerer: in Layerer_Type)
                              return Natural is
   begin

      return Layerer.Size;

   end Get_Layer_Number;


   function Get_Position (Layerer: in Layerer_Type;
                          Layer  : not null Surface_Type)
                          return Positive is
   begin

      for I in Positive range 1 .. Layerer.Size loop
         if Layerer.Layers(I) = Layer then
            return I;
         end if;
      end loop;

      raise Ansi.Exceptions.Unknown_Layer_Issue
      with "The given layer wasn't found in the layerer!";

   end Get_Position;


   function Get_Layer (Layerer : in Layerer_Type;
                       Position: Positive)
                       return not null Surface_Type is
   begin

      if Position > Layerer.Size then
         raise Ansi.Exceptions.Out_Of_Bounds_Issue
         with "The position of the layer is out of bounds!";
      end if;

      return Layerer.Layers(Position);

   end Get_Layer;


   function Get_Visibility (Layerer: in Layerer_Type;
                            Layer  : Surface_Type)
                            return Boolean is
   begin

      for I in Positive range 1 .. Layerer.Size loop
         if Layerer.Layers(I) = Layer then
            return Layerer.Visible(I);
         end if;
      end loop;

      raise Ansi.Exceptions.Unknown_Layer_Issue
      with "The given layer wasn't found in the layerer!";

   end Get_Visibility;


   function Get_Visibility (Layerer : in Layerer_Type;
                            Position: Positive)
                            return Boolean is
   begin

      if Position > Layerer.Size then
         raise Ansi.Exceptions.Out_Of_Bounds_Issue
         with "The position of the layer is out of bounds!";
      end if;

      return Layerer.Visible(Position);

   end Get_Visibility;

end Ansi.Surfaces;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
