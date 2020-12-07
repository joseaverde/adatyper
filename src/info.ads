-------------------------------------------------------------------------------
--                                                                           --
--                              I N F O . A D S                              --
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

with Encoding;

--
-- @summary
-- This package contains programme information.
--
-- @description
-- This package contains constants, variables and more about the programme
-- information like version, the name, the author, and so on.
--
package Info is

   -- The `Author' name, it's my name. If you fork or anything this project,
   -- please at least give some recognition to the original author.
   -- TODO: As my name contains non-ASCII characters I use the encoding package
   Author : constant String := "Jos" & Encoding.Convert(Encoding.LC_E_Acute) &
                                       " " &
                               "Antonio" &
                                       " " &
                               "Verde" &
                                       " " &
                               "Jim" & Encoding.Convert(Encoding.LC_E_Acute) &
                               "nez";

   -- This type is used to give which version is the project at. This is not
   -- that importat right now. But maybe in the future it might be useful if
   -- the structure of configuration files or save files have changed.
   --
   -- @value MAJOR
   -- The Major version. This value is increased when a lot of major changes
   -- have made. For example version 1.0.0 is the version when the game is
   -- completed.
   --
   -- @value MINOR
   -- The minor version. This value is increased when some minor changes have
   -- been made. For example finishing a package or adding a major feature.
   --
   -- @value PATCH
   -- This value is increased when a Bug fix has been made or a very minor
   -- change has been made.
   --
   type Version_Kind is (MAJOR, MINOR, PATCH);

   -- The current version. If hope I don't forget to change it every release.
   Version: constant array (Version_Kind'Range) of Natural := (MAJOR => 0,
                                                               MINOR => 0,
                                                               PATCH => 3);

   -- Whether this is a prerelease.
   Pre_Release: constant Boolean := True;

   -- The Version but converted into a string.
   Version_Str: constant String := "v0.0.3";

   -- The name of the programme, in this case AdaTyper.
   Programme_Name: constant String := "AdaTyper";

   -- This is the programme name plus the version.
   Programme_Full_Name: constant String := Programme_Name & ' ' & Version_Str;

end Info;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
