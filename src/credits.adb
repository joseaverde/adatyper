-------------------------------------------------------------------------------
--                                                                           --
--                           C R E D I T S . A D B                           --
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

with Ada.Command_Line;
with Ada.Text_IO;

package body Credits is

   procedure Startup_Notice is
      procedure P(Item: String) renames Ada.Text_IO.Put_Line;
      C: Character;
   begin
      P("AdaTyper Copyright (c) 2020-2022 José Antonio Verde Jiménez");
      P("This program comes with ABSOLUTELY NO WARRANTY; for details see the");
      P("`Warranty'  subsection  inside the  `Credits'  menu or  running the");
      P("program with the `--warranty' flag:");
      P("   " & Ada.Command_Line.Command_Name & " --warranty");
      P("This is free software software, and you are welcome to redistribute");
      P("it under certain conditions; see the  `Redistribute it'  subsection");
      P("inside  the   `Credits'  menu  or  running  the  program  with  the");
      P("`--conditions' flag:");
      P("   " & Ada.Command_Line.Command_Name & " --conditions");
      P("");
      P("In other languages the subsections' names inside the `Credits' menu");
      P("may change but not the command line arguments.");
      P("You can also  read the copy of the  GNU General Public Licence  you");
      P("must have received with this piece of software. If not see:");
      P("   <https://www.gnu.org/licenses/>   ");
      P("");
      P("      Warm your fingers up and press any key to continue...");
      Ada.Text_IO.Get_Immediate(C);
      Ada.Text_IO.Put_Line(ASCII.ESC & "[2J");
   end Startup_Notice;

end Credits;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
