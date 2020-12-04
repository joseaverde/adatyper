-------------------------------------------------------------------------------
--                                                                           --
--                       A N S I - C U R S O R . A D B                       --
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
with Ansi.Exceptions;


package body Ansi.Cursors is


   --------------------------------------
   -- CURSOR CREATION AND DEALLOCATION --
   --------------------------------------
   
   function New_Cursor (Row: Row_Type;
                        Col: Col_Type)
                        return not null Ansi.Cursor_Type is
   begin
      
      return new Cursor_Type'(Row => Row,
                              Col => Col);

   end New_Cursor;


   procedure Free (Cursor: in out Ansi.Cursor_Type) is
      procedure Deallocate is new Ada.Unchecked_Deallocation(Cursor_Type,
                                                             Ansi.Cursor_Type);
   begin

      Deallocate(Cursor);

   end Free;



   -----------------------
   -- CURSOR OPERATIONS --
   -----------------------
 
   function Set_Position (Cursor : in out Cursor_Type;
                          New_Row: Row_Type;
                          New_Col: Col_Type)
                          return Str_Type is
   begin

      Cursor.Row := New_Row;
      Cursor.Col := New_Col;

      return Ansi.Compliance.Set_Position_Ret(Row => New_Row,
                                              Col => New_Col);

   end Set_Position;

   
   procedure Set_Position (Cursor : in out Cursor_Type;
                           New_Row: Row_Type;
                           New_Col: Col_Type;
                           Move   : Boolean := True) is
   begin
      
      if Move then
         Ansi.Compliance.Set_Position(Row => New_Row,
                                      Col => New_Col);
      end if;
      Cursor.Row := New_Row;
      Cursor.Col := New_Col;

   end Set_Position;


   procedure Set_Position (Cursor : in out Cursor_Type;
                           Cursor2: in     Ansi.Cursor_Type;
                           Move   : Boolean := True) is
   begin

      if Move then
         Ansi.Compliance.Set_Position(Row => Cursor2.Row,
                                      Col => Cursor2.Col);
      end if;
      Cursor.Row := Cursor2.Row;
      Cursor.Col := Cursor2.Col;

   end Set_Position;


   procedure Set_Row (Cursor : in out Cursor_Type;
                      New_Row: Row_Type;
                      Move   : Boolean := True) is
   begin
      
      if Move then
         Ansi.Compliance.Set_Position(Row =>    New_Row,
                                      Col => Cursor.Col);
      end if;
      Cursor.Row := New_Row;

   end Set_Row;
  

   procedure Set_Col (Cursor : in out Cursor_Type;
                      New_Col: Col_Type;
                      Move   : Boolean := True) is
   begin
      
      if Move then
         Ansi.Compliance.Set_Position(Row => Cursor.Row,
                                      Col =>    New_Col);
      end if;
      Cursor.Col := New_Col;
      
   end Set_Col;



   procedure Get_Position (Cursor : in  Cursor_Type;
                           Out_Row: out Row_Type;
                           Out_Col: out Col_Type) is
   begin

      Out_Row := Cursor.Row;
      Out_Col := Cursor.Col;

   end Get_Position;


   function Get_Row (Cursor: in Cursor_Type)
                     return Row_Type is
   begin

      return Cursor.Row;

   end Get_Row;


   function Get_Col (Cursor: in Cursor_Type)
                     return Col_Type is
   begin
      
      return Cursor.Col;

   end Get_Col;



   procedure Move_Up (Cursor: in out Cursor_Type;
                      Rows  : Positive := 1;
                      Move  : Boolean  := True) is
   begin
      
      if Cursor.Row <= Row_Type(Rows) then
         raise Ansi.Exceptions.Out_Of_Bounds_Issue
         with "Moving the cursor too much up!";
      end if;

      if Move then
         Ansi.Compliance.Move_Up(Rows);
      end if;
      Cursor.Row := Cursor.Row - Row_Type(Rows);

   end Move_Up;


   procedure Move_Down (Cursor: in out Cursor_Type;
                        Rows  : Positive := 1;
                        Move  : Boolean  := True) is
   begin

      if Move then
         Ansi.Compliance.Move_Down(Rows);
      end if;
      Cursor.Row := Cursor.Row + Row_Type(Rows);

   end Move_Down;


   procedure Move_Right (Cursor: in out Cursor_Type;
                         Cols  : Positive := 1;
                         Move  : Boolean  := True) is
   begin
      
      if Move then
         Ansi.Compliance.Move_Right(Cols);
      end if;
      Cursor.Col := Cursor.Col + Col_Type(Cols);

   end Move_Right;


   procedure Move_Left (Cursor: in out Cursor_Type;
                        Cols  : Positive := 1;
                        Move  : Boolean  := True) is
   begin
      
      if Cursor.Col <= Col_Type(Cols) then
         raise Ansi.Exceptions.Out_Of_Bounds_Issue
         with "Moving the cursor too much to the left!";
      end if;

      if Move then
         Ansi.Compliance.Move_Left(Cols);
      end if;
      Cursor.Col := Cursor.Col - Col_Type(Cols);

   end Move_Left;   


end Ansi.Cursors;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
