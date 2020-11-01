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
      
      if Surf.Cursor.Get_Col > Col_Type(Surf.Width) then
         raise Ansi.Exceptions.Out_Of_Bounds_Issue
         with "Character went out of bounds!";
      end if;

      Surf.Grid(Surf.Cursor.Get_Row, Surf.Cursor.Get_Col).Char := Item;
      Surf.Push(Surf.Cursor);
      Surf.Cursor.Move_Right(1);
      
   end Put;
   
   
   procedure Put (Surface: Surface_Access := null) is
      Item: Element;
   begin

      Main_Cursor.Set_Position(Surface.Cursor.Get_Row, Surface.Cursor.Get_Col);
      for Y in Row_Type range 1 .. Surface.Cursor.Get_Row loop
         for X in Col_Type range 1 .. Surface.Cursor.Get_Col loop
            Item := Surface.Grid(Y, X);
            --Ansi.Colors.Put_Foreground(Color  => Item.Fmt.Fg_Color,
            --                           Bright => Item.Fmt.Fg_Bright);
            --Ansi.Colors.Put_Background(Color  => Item.Fmt.Bg_Color,
            --                           Bright => Item.Fmt.Bg_Color);
            --Ansi.Styles.Put_Styles(Styles => Item.Fmt.Styles);
            --Ansi.Text_IO.Put_Char(Item.Char);
         end loop;
         Main_Cursor.Move_Down(1);
         Main_Cursor.Set_Col(1);
      end loop;

   end Put;

end Ansi.Surfaces;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
