-------------------------------------------------------------------------------
--                                                                           --
--                     A N S I - O S _ U T I L S . A D B                     --
--                                 P O S I X                                 --
--                                                                           --
--                              A D A T Y P E R                              --
--                                                                           --
--                                  S P E C                                  --
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
with Ansi.Text_IO;


package body Ansi.Os_Utils is

   ------------
   -- SYSTEM --
   ------------

   function System_Command (Cmd: String)
                            return Boolean is
   begin

      return C_System(Interfaces.C.To_C(Cmd)) = 0;

   end System_Command;


   --------------
   -- TERMINAL --
   --------------

   procedure Prepare is
   begin

      -- We add many new lines in order not to overwrite what is already
      -- written.
      for Row in Row_Type range 1 .. Height loop
         Ansi.Text_IO.Put_Ansi_Sequence("" & Char_Type'Val(10));
      end loop;

   -- if not System_Command("tput smcup") or
      if not System_Command("stty -echo") or
         not System_Command("tput civis") or
      not True then

         Clean_Up;
         raise Ansi.Exceptions.Initialization_Issue
         with "Couldn't prepare the terminal!";

      end if;

   end Prepare;


   procedure Clean_Up is
      Temp: Boolean;
   begin
      
      Temp := System_Command("stty echo ");
      Temp := System_Command("tput cnorm");
   -- Temp := System_Command("tput rmcup");

   end Clean_Up;
   
   -----------
   -- IOCTL --
   -----------

   procedure Update_Terminal_Size is
      Ws        : Winsize;
      Temp_Int  : Interfaces.C.int;
   begin

      Temp_Int := Ioctl(Fd      => 1,  -- File descriptor = 1 (Standard output)
                        Request => TIOCGWINSZ,
                        Struct  => Ws);

      Height := Row_Type(Ws.ws_row);
      Width  := Col_Type(Ws.ws_col);

   end Update_Terminal_Size;


end Ansi.Os_Utils;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
