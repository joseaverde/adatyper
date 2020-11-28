-------------------------------------------------------------------------------
--                                                                           --
--                         T E S T _ A N S I . A D B                         --
--                                                                           --
--                              A D A T Y P E R                              --
--                                                                           --
--                                  M A I N                                  --
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

with Ansi;
with Tests;

function Test_Ansi return Natural is

   use type Ansi.Row_Type;
   use type Ansi.Col_Type;
   
   Row    : Ansi.Row_Type := 2;
   Col    : Ansi.Col_Type := 3;
   Cursor : Ansi.Cursor_Type;
   Surface: Ansi.Surface_Type (10, 10);
   Color  : Ansi.Color_Type;
   Style  : Ansi.Style_Type;
   Styles : Ansi.Style_Array;
   Char   : Ansi.Char_Type;
   Str    : Ansi.Str_Type (1 .. 10);

   Status : Boolean;

begin


   -- Testing setting the position.
   Ansi.Set_Position (Surface => Surface,
                      Row     => Row,
                      Col     => Col);

   Ansi.Clear;

   Ansi.Finalize;
   Tests.Finalize;

   return 0;

exception
   when Error: others =>
      Tests.Error(Error);
      return 255;

end Test_Ansi;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
