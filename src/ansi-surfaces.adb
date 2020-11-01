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
with Ansi.Exceptions;
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
      Surf.Push(Surf.Cursor);
      Surf.Cursor.Move_Right(1);
      
   end Put;
   
   
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
      for Y in Row_Type range 1 .. Surf.Height loop
         for X in Col_Type range 1 .. Surf.Width loop
            Item := Surf.Grid(Y, X);
            --Ansi.Colors.Put_Foreground(Color  => Item.Fmt.Fg_Color,
            --                           Bright => Item.Fmt.Fg_Bright);
            --Ansi.Colors.Put_Background(Color  => Item.Fmt.Bg_Color,
            --                           Bright => Item.Fmt.Bg_Color);
            --Ansi.Styles.Put_Styles(Styles => Item.Fmt.Styles);
           -- Ada.Text_IO.Put(Char_Type'Pos(Item.Char)'Image);
            Ansi.Text_IO.Put(Item.Char);
         end loop;
         Main_Cursor.Move_Down(1);
         Main_Cursor.Set_Col(1);
      end loop;

   end Put;


end Ansi.Surfaces;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
