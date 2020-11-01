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

package body Ansi.Text_IO is

   procedure Put (Item: Char_Type) is
   begin

      Ada.Wide_Text_IO.Put(Wide_Character(Item));
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
       --  Ada.Wide_Text_IO.Put(Wide_Character(Char));
      end loop;

   end Put;



   procedure Put_Ansi_Sequence (Item: Str_Type) is
   begin

      for Char of Item loop
         Ada.Wide_Text_IO.Put(Wide_Character(Char));
      end loop;

   end Put_Ansi_Sequence;

end Ansi.Text_IO;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
