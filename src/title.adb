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
with Ansi.Styles;
with Ansi.Text_IO;
with Ada.Text_IO; --DEBUG

package body Title is

   use type Ansi.Col_Type;
   use type Ansi.Row_Type;
   use type Ansi.Surfaces.Layerer_Type;

   procedure Main_Title is

      Left: Ansi.Col_Type;
      Top : Ansi.Row_Type;

      Cursor_Pos: Natural := 0;
      Sleep     : Duration := 1.0;

      procedure Update_Positions is
      begin
         
         Left := (Ansi.Get_Width - 8*7) / 2;
         Top  := 4;  -- Ansi.Row_Type(Left) / 4;

         for P in Positive range 1 .. 8 loop
            Ansi.Set_Position(Surface => Letterer.Get_Layer(P),
                              Row     => Top,
                              Col     => Ansi.Col_Type(
                                          Positive(Left) +
                                          (P - 1) * 7));
         end loop;
         
      end Update_Positions;

   begin

      Ansi.Clear;
      Update_Positions;

      for P in Positive range 1 .. 8 loop
         Letterer.Show(P);
         delay Sleep;
         Letterer.Update;
         Ansi.Surfaces.Put(Ansi.Get_Main_Surface);
      end loop;

   -- Ansi.Surfaces.Put(  Letter_Big_A, Positions(1).R, Positions(1).C);
   -- Ansi.Surfaces.Put(Letter_Small_d, Positions(2).R, Positions(2).C);
   -- Ansi.Surfaces.Put(Letter_Small_a, Positions(3).R, Positions(3).C);
   -- Ansi.Surfaces.Put(  Letter_Big_T, Positions(4).R, Positions(4).C);
   -- Ansi.Surfaces.Put(Letter_Small_y, Positions(5).R, Positions(5).C);
   -- Ansi.Surfaces.Put(Letter_Small_p, Positions(6).R, Positions(6).C);
   -- Ansi.Surfaces.Put(Letter_Small_e, Positions(7).R, Positions(7).C);
   -- Ansi.Surfaces.Put(Letter_Small_r, Positions(8).R, Positions(8).C);

   exception
      when Constraint_Error =>
         raise Ansi.Exceptions.Windows_Size_Issue
         with "The window is too small!";
   end Main_Title;

begin

   -- We change the colour of the letters.
   Ansi.Colors.Set_Foreground(  Letter_Big_A,  Ansi.Red,  True);
   Ansi.Colors.Set_Foreground(Letter_Small_d,  Ansi.Red,  True);
   Ansi.Colors.Set_Foreground(Letter_Small_a,  Ansi.Red,  True);
   Ansi.Colors.Set_Foreground(  Letter_Big_T,  Ansi.Red,  True);
   Ansi.Colors.Set_Foreground(Letter_Small_y,  Ansi.Red,  True);
   Ansi.Colors.Set_Foreground(Letter_Small_p,  Ansi.Red,  True);
   Ansi.Colors.Set_Foreground(Letter_Small_e,  Ansi.Red,  True);
   Ansi.Colors.Set_Foreground(Letter_Small_r,  Ansi.Red,  True);
   Ansi.Colors.Set_Foreground(Letter_Cursor , Ansi.White, True);

   -- We change some styles.
   Ansi.Styles.Set_Style(Letter_Big_A, Ansi.Slow_Blink);

   -- We add them to the letterer.
   Letterer.Add(  Letter_Big_A);
   Letterer.Add(Letter_Small_d);
   Letterer.Add(Letter_Small_a);
   Letterer.Add(  Letter_Big_T);
   Letterer.Add(Letter_Small_y);
   Letterer.Add(Letter_Small_p);
   Letterer.Add(Letter_Small_e);
   Letterer.Add(Letter_Small_r);
   Letterer.Add(Letter_Cursor );

   -- We finally hide them all, but the cursor.
   Letterer.Hide(  Letter_Big_A);
   Letterer.Hide(Letter_Small_d);
   Letterer.Hide(Letter_Small_a);
   Letterer.Hide(  Letter_Big_T);
   Letterer.Hide(Letter_Small_y);
   Letterer.Hide(Letter_Small_p);
   Letterer.Hide(Letter_Small_e);
   Letterer.Hide(Letter_Small_r);
   Letterer.Hide(Letter_Cursor );

end Title;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
