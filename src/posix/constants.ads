-------------------------------------------------------------------------------
--                                                                           --
--                         C O N S T A N T S . A D S                         --
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

with Ada.Interrupts;
with Ada.Interrupts.Names;

--
-- @summary
-- This package contains system specific constants.
--
-- @description
-- This package contains posix specific constants that are only available here,
-- for example, the SIGWINCH signal is used to handle when the terminal has
-- been resized, which isn't available by default on windows.
--
package Constants is

   -- This is signal is given when the terminal has been resized.
   SIGWINCH: CONSTANT Ada.Interrupts.Interrupt_ID :=
                                                Ada.Interrupts.Names.SIGWINCH;

end Constants;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
