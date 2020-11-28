-------------------------------------------------------------------------------
--                                                                           --
--                             D E B U G . A D S                             --
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

-- @summary
-- This package contains debugging functions.
--
-- @description
-- This package contains debugging functions because it's hard to use a
-- debugger in this kind of pseudographical interfaces where there are too much
-- things printed onto the screen.
--
package Debug is

   --
   -- This procedure adds a kind of breakpoint with information about what is
   -- happening in that part of the code and ultimately wait for a character to
   -- be pressed to continue the code, this way, it's much more controlled.
   --
   -- @param Text
   -- The text to be printed onto the screen when the breakpoint is reached.
   --
   -- @param Col
   -- The column to write the text (a.k.a, tabulation). 1 is the left margin.
   --
   -- @param Stop
   -- If this parameter is True, then the programme waits for the developer
   -- input of any character to continue.
   --
   procedure Breakpoint (Text: String   := "";
                         Col : Positive := 1;
                         Stop: Boolean  := False);

end Debug;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
