-------------------------------------------------------------------------------
--                                                                           --
--                              M A I N . A D B                              --
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

with Ada.Text_IO;
with Ansi;
with Ansi.Colors;
with Ansi.Styles;
with Ansi.Surfaces;
with Credits;

-- This is the main function of the program, it returns an natural nuber with
-- the error code if any occurs.
function Main return Natural is
begin
   
   Ansi.Colors.Set_Color(Ansi.Colors.Blue, Ansi.Colors.Yellow, True);
   Ansi.Styles.Set_Style(Ansi.Styles.Reversed);
   Ansi.Styles.Set_Style(Ansi.Styles.Underline);
   Ansi.Styles.Set_Style(Ansi.Styles.Italic);
   Ansi.Styles.Remove_All_Styles;
   Ansi.Colors.Plain;
   Ansi.Colors.Set_Foreground_Color(Ansi.Colors.Green, Ansi.Colors.Bright);
   Ansi.Colors.Set_Background_Color(Ansi.Colors.Black, False);
   Ansi.Styles.Set_Style(Ansi.Styles.Bright);
   Ansi.Styles.Set_Style(Ansi.Styles.Italic);
   Credits.Startup_Notice;

   Ansi.Initialize;
   Ada.Text_IO.Put_Line("Height:" & Ansi.Get_Height'Image);
   Ada.Text_IO.Put_Line("Width: " & Ansi.Get_Width'Image);

   return 0;

end Main;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
