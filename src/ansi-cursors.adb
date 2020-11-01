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


with  Ada.Text_IO;
with Ansi.Text_IO;
with Ansi.Exceptions;

package body Ansi.Cursors is

   procedure Put_Ansi_Sequence (Item: Str_Type)
      renames Ansi.Text_IO.Put_Ansi_Sequence;
   
   -- This function transforms a number into a string.
   function To_String (N: Positive)
                       return Str_Type is
      C: String  := N'Image;
      S: Str_Type (1 .. C'Length);
      L: Natural := 0;
   begin

      for I of C loop
         if I /= ' ' then
            S(S'First + L) := Char_Type'Val(Character'Pos(I));
         end if;
      end loop;

      return S(S'First .. S'First + L);

   end To_String;

   ----------------------------------------------------------------------------
   
   
   procedure Set_Position (Cursor : in out Cursor_Type;
                           New_Row: Row_Type;
                           New_Col: Col_Type;
                           Move   : Boolean := True) is
   begin
      
      if Move then
         Put_Ansi_Sequence(ESC & To_String(Positive(New_Row)) &
                           ";" & To_String(Positive(New_Col)) & "H");
      end if;
      Cursor.Row := New_Row;
      Cursor.Col := New_Col;

   end Set_Position;


   procedure Set_Position (Cursor : in out Cursor_Type;
                           Cursor2: in     Cursor_Type;
                           Move   : Boolean := True) is
   begin

      if Move then
         Put_Ansi_Sequence(ESC & To_String(Positive(Cursor2.Row)) &
                           ";" & To_String(Positive(Cursor2.Col)) & "H");
      end if;
      Cursor.Row := Cursor2.Row;
      Cursor.Col := Cursor2.Col;

   end Set_Position;


   procedure Set_Row (Cursor : in out Cursor_Type;
                      New_Row: Row_Type;
                      Move   : Boolean := True) is
   begin
      
      if Move then
         Put_Ansi_Sequence(ESC & To_String(Positive(   New_Row)) &
                           ";" & To_String(Positive(Cursor.Col)) & "H");
      end if;
      Cursor.Row := New_Row;

   end Set_Row;
  

   procedure Set_Col (Cursor : in out Cursor_Type;
                      New_Col: Col_Type;
                      Move   : Boolean := True) is
   begin
      
      if Move then
         Put_Ansi_Sequence(ESC & To_String(Positive(Cursor.Row)) &
                          ";" & To_String(Positive(   New_Col)) & "H");
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
      
      if Move then
         Put_Ansi_Sequence(ESC & To_String(Rows) & "A");
      end if;
      Cursor.Row := Cursor.Row - Row_Type(Rows);

   exception
      when Constraint_Error =>
         raise Ansi.Exceptions.Out_Of_Bounds_Issue
         with "Moving the cursor too much up!";
   end Move_Up;


   procedure Move_Down (Cursor: in out Cursor_Type;
                        Rows  : Positive := 1;
                        Move  : Boolean  := True) is
   begin
      
      if Move then
         Put_Ansi_Sequence(ESC & To_String(Rows) & "B");
      end if;
      Cursor.Row := Cursor.Row + Row_Type(Rows);

   end Move_Down;


   procedure Move_Right (Cursor: in out Cursor_Type;
                         Cols  : Positive := 1;
                         Move  : Boolean  := True) is
   begin
      
      if Move then
         Put_Ansi_Sequence(ESC & To_String(Cols) & "C");
      end if;
      Cursor.Col := Cursor.Col + Col_Type(Cols);

   end Move_Right;


   procedure Move_Left (Cursor: in out Cursor_Type;
                        Cols  : Positive := 1;
                        Move  : Boolean  := True) is
   begin
      
      if Move then
         Put_Ansi_Sequence(ESC & To_String(Cols) & "D");
      end if;
      Cursor.Col := Cursor.Col - Col_Type(Cols);

   exception
      when Constraint_Error =>
         raise Ansi.Exceptions.Out_Of_Bounds_Issue
         with "Moving the cursor too much to the left!";

   end Move_Left;   


end Ansi.Cursors;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
