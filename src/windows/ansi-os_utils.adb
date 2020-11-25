-------------------------------------------------------------------------------
--                                                                           --
--                     A N S I - O S _ U T I L S . A D B                     --
--                               W I N D O W S                               --
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

with Ansi.Text_IO;


package body Ansi.Os_Utils is

   procedure Prepare is
      procedure Set_Up_Console;
      pragma Import (C, Set_Up_Console, "setupConsole");
   begin
   
      Set_Up_Console;
      -- We add many new lines in order not to overwrite what has already been
      -- written.
      for Row in Row_Type range 1 .. Height loop
         Ansi.Text_IO.Put_Ansi_Sequence("" & Char_Type'Val(10));
      end loop;

      -- TODO Add errors and complete it.
      
   end Prepare;


   procedure Clean_Up is
      procedure Restore_Console;
      pragma Import (C, Restore_Console, "restoreConsole");
   begin

      Restore_Console;
   
   end Clean_Up;

   
   -- TODO
   function Update_Terminal_Size return Boolean is
   begin

      Height := 24;
      Width  := 80;

      return False;

   end Update_Terminal_Size;

end Ansi.Os_Utils;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
