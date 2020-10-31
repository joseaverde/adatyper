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


with Ansi.Exceptions;


package body Ansi.Surfaces is
   
   
   procedure Put (Item   : Str_Type;
                  Surface: Surface_Access := null) is
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
                  Surface: Surface_Access := null) is
      Surf: Surface_Access := (if Surface = null then
                                 Main_Surface
                               else
                                 Surface);
   begin
      
      if Surf.Cursor.Col > Surf.Width then
         raise Ansi.Exceptions.Out_Of_Bounds_Issue
         with "Character went out of bounds!";
      end if;

      Surf.Grid(Surf.Cursor.Row, Surf.Cursor.Col).Char := Item;
      Surf.Push(Surf.Cursor.Row, Surf.Cursor.Col);
      Surf.Cursor.Row := Surf.Cursor.Row + 1;
      Surf.Cursor.Col := Surf.Cursor.Col + 1;
      
   end Put;
   
   
   -- TODO: Finish this.
   procedure Put (Surface: Surface_Access := null;
                  Update : Boolean        := False) is
   begin

      null;

   end Put;

end Ansi.Surfaces;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
