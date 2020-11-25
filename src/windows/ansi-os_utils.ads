-------------------------------------------------------------------------------
--                                                                           --
--                     A N S I - O S _ U T I L S . A D S                     --
--                               W I N D O W S                               --
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

with Interfaces.C;

-- This package contains some os specific functions from C in the Windows
-- operating system.
private package Ansi.Os_Utils is

   -------------
   -- CONSOLE --
   -------------

   -- This procedure prepares the console.
   procedure Prepare;

   -- This procedure cleans up and restores the console.
   procedure Clean_Up;
   
   ---------------
   -- WINDOWS.H --
   ---------------
   
   -- This function updates the terminal size.
   function Update_Terminal_Size return Boolean;
   pragma Inline (Update_Terminal_Size);


end Ansi.Os_Utils;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
