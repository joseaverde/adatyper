-------------------------------------------------------------------------------
--                                                                           --
--                         C O N S T A N T S . A D S                         --
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

with Ada.Interrupts;
with Ada.Interrupts.Names;

--
-- @summary
-- This package contains system specific constants.
--
-- @description
-- This package contains constants which are available in linux but not in
-- windows like the signal when the console has been resized.
--
package Constants is

   -- TODO: Find the windows console resized signal.
   SIGWINCH: CONSTANT Ada.Interrupts.Interrupt_ID :=
                                                   Ada.Interrupts.Names.SIGFPE;

end Constants;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
