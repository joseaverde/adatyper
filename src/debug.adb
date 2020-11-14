-------------------------------------------------------------------------------
--                                                                           --
--                             D E B U G . A D B                             --
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

with Ada.Text_IO;

package body Debug is

   procedure Breakpoint (Text: String   := "";
                         Col : Positive := 1;
                         Stop: Boolean  := False) is
      Char: Character;
   begin
      
      Ada.Text_IO.Set_Col(File => Ada.Text_IO.Standard_Error,
                          To   => Ada.Text_IO.Count(Col));
      Ada.Text_IO.Put_Line(File => Ada.Text_IO.Standard_Error,
                           Item => ASCII.ESC & "[94m[DEBUG] " &
                                   ASCII.ESC & "[0m" &
                                   ASCII.ESC & "[2m" & Text   &
                                   ASCII.ESC & "[0m");

      if Stop then
         Ada.Text_IO.Get_Immediate(Char);
      end if;

   end Breakpoint;

end Debug;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
