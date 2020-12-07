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
-- with Debug; use Debug;
with Info;
with Interfaces.C;


package body Ansi.Os_Utils is

   use type Str_Type;

   procedure Prepare is
      procedure C_Driver_Set_Up_Console;
      pragma Import (C, C_Driver_Set_Up_Console, "setupConsole");

      procedure C_Driver_Set_Console_Title (Title: Interfaces.C.Char_Array);
      pragma Import (C, C_Driver_Set_Console_Title, "setConsoleTitle");
   begin
   
      C_Driver_Set_Up_Console;
      C_Driver_Set_Console_Title(Interfaces.C.To_C(Info.Programme_Full_Name));
      -- We add many new lines in order not to overwrite what has already been
      -- written.
      for Row in Row_Type range 1 .. Height loop
         Ansi.Text_IO.Put_Ansi_Sequence("" & Char_Type'Val(10));
      end loop;

      Update_Terminal_Size;
      
   end Prepare;


   procedure Clean_Up is
      procedure C_Driver_Restore_Console;
      pragma Import (C, C_Driver_Restore_Console, "restoreConsole");
   begin

      C_Driver_Restore_Console;
   
   end Clean_Up;

   
   procedure Update_Terminal_Size is
      procedure C_Driver_Get_Console_Screen_Size (rows:out Interfaces.C.short;
                                                  cols:out Interfaces.C.short);
      pragma Import (C,
                     C_Driver_Get_Console_Screen_Size,
                     "getConsoleScreenSize");

      rows, cols: Interfaces.C.short;
   begin

      C_Driver_Get_Console_Screen_Size (rows => rows,
                                        cols => cols);
      Height := Row_Type(rows);
      Width  := Col_Type(cols);

   end Update_Terminal_Size;

end Ansi.Os_Utils;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
