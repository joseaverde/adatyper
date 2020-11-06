-------------------------------------------------------------------------------
--                                                                           --
--                             T I T L E . A D B                             --
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

with Ansi.Colors;
with Ansi.Cursors;
with Ansi.Exceptions;
with Ansi.Text_IO;

package body Title is

   use type Ansi.Col_Type;
   use type Ansi.Row_Type;

   procedure Main_Title is
      type Position is
         record
            R: Ansi.Row_Type;
            C: Ansi.Col_Type;
         end record;

      Left: Ansi.Col_Type;
      Top : Ansi.Row_Type;

      Positions : array (1 .. 8) of Position;
      Cursor_Pos: Natural := 0;
      Sleep     : Duration := 1.0;
   begin

      Left := (Ansi.Get_Width - 8*7) / 2;
      Top  := 4; -- Ansi.Row_Type(Left) / 4;

      for P in Positions'Range loop
         Positions(P) := Position'(Top, Ansi.Col_Type(Positive(Left)+(P-1)*7));
      end loop;

      Ansi.Surfaces.Put(  Letter_Big_A, Positions(1).R, Positions(1).C);
      Ansi.Surfaces.Put(Letter_Small_d, Positions(2).R, Positions(2).C);
      Ansi.Surfaces.Put(Letter_Small_a, Positions(3).R, Positions(3).C);
      Ansi.Surfaces.Put(  Letter_Big_T, Positions(4).R, Positions(4).C);
      Ansi.Surfaces.Put(Letter_Small_y, Positions(5).R, Positions(5).C);
      Ansi.Surfaces.Put(Letter_Small_p, Positions(6).R, Positions(6).C);
      Ansi.Surfaces.Put(Letter_Small_e, Positions(7).R, Positions(7).C);
      Ansi.Surfaces.Put(Letter_Small_r, Positions(8).R, Positions(8).C);

   exception
      when Constraint_Error =>
         raise Ansi.Exceptions.Windows_Size_Issue
         with "The window is too small!";
   end Main_Title;

begin

   Ansi.Colors.Set_Cursor_Foreground(Letter_Big_A, Ansi.Red, True);
   Ansi.Surfaces.Put("     _ ", Letter_Big_A, True);
   Ansi.Surfaces.Put("    / |", Letter_Big_A, True);
   Ansi.Surfaces.Put("   /  |", Letter_Big_A, True);
   Ansi.Surfaces.Put("  / - |", Letter_Big_A, True);
   Ansi.Surfaces.Put(" / /| |", Letter_Big_A, True);
   Ansi.Surfaces.Put("/_/ |_|", Letter_Big_A, True);

   Ansi.Colors.Set_Cursor_Foreground(Letter_Small_d, Ansi.Red, True);
   Ansi.Surfaces.Put("     _ ", Letter_Small_d, True);
   Ansi.Surfaces.Put("    / |", Letter_Small_d, True);
   Ansi.Surfaces.Put("  _)  |", Letter_Small_d, True);
   Ansi.Surfaces.Put(" /    |", Letter_Small_d, True);
   Ansi.Surfaces.Put("( (_) |", Letter_Small_d, True);
   Ansi.Surfaces.Put(" \___/ ", Letter_Small_d, True);

   Ansi.Colors.Set_Cursor_Foreground(Letter_Small_a, Ansi.Red, True);
   Ansi.Surfaces.Put("       ", Letter_Small_a, True);
   Ansi.Surfaces.Put("       ", Letter_Small_a, True);
   Ansi.Surfaces.Put("  ___  ", Letter_Small_a, True);
   Ansi.Surfaces.Put(" /   \ ", Letter_Small_a, True);
   Ansi.Surfaces.Put("( (_) )", Letter_Small_a, True);
   Ansi.Surfaces.Put(" \____\", Letter_Small_a, True);


   Ansi.Colors.Set_Cursor_Foreground(Letter_Big_T, Ansi.Red, True);
   Ansi.Surfaces.Put(" _____ ", Letter_Big_T, True);
   Ansi.Surfaces.Put("(_   _)", Letter_Big_T, True);
   Ansi.Surfaces.Put("  | |  ", Letter_Big_T, True);
   Ansi.Surfaces.Put("  | |  ", Letter_Big_T, True);
   Ansi.Surfaces.Put("  | |  ", Letter_Big_T, True);
   Ansi.Surfaces.Put("  |_|  ", Letter_Big_T, True);

   Ansi.Colors.Set_Cursor_Foreground(Letter_Small_y, Ansi.Red, True);
   Ansi.Surfaces.Put("       ", Letter_Small_y, True);
   Ansi.Surfaces.Put(" _   _ ", Letter_Small_y, True);
   Ansi.Surfaces.Put("( \_/ )", Letter_Small_y, True);
   Ansi.Surfaces.Put(" \   / ", Letter_Small_y, True);
   Ansi.Surfaces.Put(" _) /  ", Letter_Small_y, True);
   Ansi.Surfaces.Put("(__/   ", Letter_Small_y, True);

   Ansi.Colors.Set_Cursor_Foreground(Letter_Small_p, Ansi.Red, True);
   Ansi.Surfaces.Put("       ", Letter_Small_p, True);
   Ansi.Surfaces.Put("  ___  ", Letter_Small_p, True);
   Ansi.Surfaces.Put(" /   \ ", Letter_Small_p, True);
   Ansi.Surfaces.Put("| (_) )", Letter_Small_p, True);
   Ansi.Surfaces.Put("|  __/ ", Letter_Small_p, True);
   Ansi.Surfaces.Put("|_(    ", Letter_Small_p, True);

   Ansi.Colors.Set_Cursor_Foreground(Letter_Small_e, Ansi.Red, True);
   Ansi.Surfaces.Put("       ", Letter_Small_e, True);
   Ansi.Surfaces.Put("  ____ ", Letter_Small_e, True);
   Ansi.Surfaces.Put(" / __ \", Letter_Small_e, True);
   Ansi.Surfaces.Put("/  ___/", Letter_Small_e, True);
   Ansi.Surfaces.Put("\ (___ ", Letter_Small_e, True);
   Ansi.Surfaces.Put(" \____)", Letter_Small_e, True);

   Ansi.Colors.Set_Cursor_Foreground(Letter_Small_r, Ansi.Red, True);
   Ansi.Surfaces.Put("       ", Letter_Small_r, True);
   Ansi.Surfaces.Put("   ___ ", Letter_Small_r, True);
   Ansi.Surfaces.Put("/\/ __)", Letter_Small_r, True);
   Ansi.Surfaces.Put("|  /   ", Letter_Small_r, True);
   Ansi.Surfaces.Put("| |    ", Letter_Small_r, True);
   Ansi.Surfaces.Put("|_|    ", Letter_Small_r, True);


   Ansi.Colors.Set_Cursor_Foreground(Letter_Cursor, Ansi.White, True);
   Ansi.Surfaces.Put("#######", Letter_Cursor, True);
   Ansi.Surfaces.Put("#######", Letter_Cursor, True);
   Ansi.Surfaces.Put("#######", Letter_Cursor, True);
   Ansi.Surfaces.Put("#######", Letter_Cursor, True);
   Ansi.Surfaces.Put("#######", Letter_Cursor, True);
   Ansi.Surfaces.Put("#######", Letter_Cursor, True);


   Ansi.Colors.Set_Cursor_Background(Letter_Space, Ansi.Black, False);
   Ansi.Surfaces.Put("       ", Letter_Space, True);
   Ansi.Surfaces.Put("       ", Letter_Space, True);
   Ansi.Surfaces.Put("       ", Letter_Space, True);
   Ansi.Surfaces.Put("       ", Letter_Space, True);
   Ansi.Surfaces.Put("       ", Letter_Space, True);
   Ansi.Surfaces.Put("       ", Letter_Space, True);

end Title;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
