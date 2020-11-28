-------------------------------------------------------------------------------
--                                                                           --
--                             T I T L E . A D S                             --
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

private with Ansi;
private with Ansi.Surfaces;

-- This package contains the title.
package Title is

   pragma Elaborate_Body (Title);

   procedure Main_Title;


private

   -- The letters
   Letter_Big_A:   Ansi.Surface_Type := Ansi.Surfaces.Create(("     _ ",
                                                              "    / |",
                                                              "   /  |",
                                                              "  / - |",
                                                              " / /| |",
                                                              "/_/ |_|"));

   Letter_Small_d: Ansi.Surface_Type := Ansi.Surfaces.Create(("     _ ",
                                                              "    / |",
                                                              "  _)  |",
                                                              " /    |",
                                                              "( (_) |",
                                                              " \___/ "));

   Letter_Small_a: Ansi.Surface_Type := Ansi.Surfaces.Create(("       ",
                                                              "       ",
                                                              "  ___  ",
                                                              " /   \ ",
                                                              "( (_) )",
                                                              " \____\"));

   Letter_Big_T  : Ansi.Surface_Type := Ansi.Surfaces.Create((" _____ ",
                                                              "(_   _)",
                                                              "  | |  ",
                                                              "  | |  ",
                                                              "  | |  ",
                                                              "  |_|  "));

   Letter_Small_y: Ansi.Surface_Type := Ansi.Surfaces.Create(("       ",
                                                              " _   _ ",
                                                              "( \_/ )",
                                                              " \   / ",
                                                              " _) /  ",
                                                              "(__/   "));

   Letter_Small_p: Ansi.Surface_Type := Ansi.Surfaces.Create(("       ",
                                                              "  ___  ",
                                                              " /   \ ",
                                                              "| (_) )",
                                                              "|  __/ ",
                                                              "|_(    "));

   Letter_Small_e: Ansi.Surface_Type := Ansi.Surfaces.Create(("       ",
                                                              "  ____ ",
                                                              " / __ \",
                                                              "/  ___/",
                                                              "\ (___ ",
                                                              " \____)"));

   Letter_Small_r: Ansi.Surface_Type := Ansi.Surfaces.Create(("       ",
                                                              "   ___ ",
                                                              "/\/ __)",
                                                              "|  /   ",
                                                              "| |    ",
                                                              "|_|    "));

   Letter_Cursor : Ansi.Surface_Type := Ansi.Surfaces.Create(("#######",
                                                              "#######",
                                                              "#######",
                                                              "#######",
                                                              "#######",
                                                              "#######"));


   -- The letterer
   Letterer: Ansi.Surfaces.Layerer_Type;

end Title;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
