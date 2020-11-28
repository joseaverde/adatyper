-------------------------------------------------------------------------------
--                                                                           --
--                      A N S I - T E X T _ I O . A D B                      --
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

with Ada.Wide_Text_IO;
with Ansi.Cursors;
with Ansi.Exceptions;
-- with Debug; use Debug;

package body Ansi.Text_IO is

   package Ada_Text_IO renames Ada.Wide_Text_IO;

   procedure Flush is
   begin

      Ada_Text_IO.Flush(File => Ada.Wide_Text_IO.Standard_Output);

   end Flush;


   procedure Put (Item: Char_Type) is
   begin

      Ada_Text_IO.Put(Wide_Character(Item));
      Main_Cursor.Move_Right(1, False);
      
      -- If we have reached the maximum width, we move to the next line, no
      -- error is raised. We suppose the Width is up-to-date, because checking
      -- for the width everytime a character is printed is very expensive.
      if Main_Cursor.Get_Col > Width then
         Main_Cursor.Move_Down;
         Main_Cursor.Set_Col(1);
      end if;

   end Put;

   

   procedure Put (Item: Str_Type) is
   begin

      for Char of Item loop
         Ansi.Text_IO.Put(Char);
      end loop;

   end Put;



   procedure Put_Ansi_Sequence (Item: Str_Type) is
   begin

      for Char of Item loop
         Ada_Text_IO.Put(Wide_Character(Char));
      end loop;

   end Put_Ansi_Sequence;


   function Get_Input (Surface: Surface_Type := null)
                       return Str_Type is
      Surf: CONSTANT Surface_Type := (if Surface = null then
                                          Main_Surface
                                      else
                                          Surface);
   begin

      return Surf.Grid(1, 1).Char & "";   -- TODO

   end Get_Input;


   ESCAPE: CONSTANT Char_Type := Char_Type'Val(Character'Pos(ASCII.ESC));
   function Get_Key (Key: out Key_Type)
                     return Boolean is
      Buffer   : Char_Type;
      Available: Boolean;
   begin
      
      Ada_Text_IO.Get_Immediate(Item      => Buffer,
                                Available => Available);

      if Available then
         if Buffer = ESCAPE then
            -- TODO: Finish this.
            Key.Kind := Special;
         else
            Key.Char := Buffer;
            Key.Kind := Ordinary;
         end if;
         return True;

      else
         return False;
      end if;

   end Get_Key;


   function Get_Char (Key: in Key_Type)
                      return Char_Type is
   begin

      if Key.Kind /= Ordinary then
         raise Ansi.Exceptions.Wrong_Kind_Of_Key_Issue
         with "The pressed key is a character!";
      end if;

      return Key.Char;

   end Get_Char;


   function Get_Code (Key: in Key_Type)
                      return Special_Key_Code is
   begin

      if Key.Kind /= Special then
         raise Ansi.Exceptions.Wrong_Kind_Of_Key_Issue
         with "The pressed key is a special key!";
      end if;

      return Key.Code;

   end Get_Code;


   function Get_Kind (Key: in Key_Type)
                      return Key_Kind is
   begin

      return Key.Kind;

   end Get_Kind;


end Ansi.Text_IO;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
