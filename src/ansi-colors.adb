-------------------------------------------------------------------------------
--                                                                           --
--                       A N S I - C O L O R S . A D S                       --
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

with Ada.Text_IO;


package body Ansi.Colors is


   procedure Set_Foreground_Color (Color  : Color_Type;
                                   Bright : Boolean := False) is
   begin

      Ada.Text_IO.Put(ESC                                &
                      (if Bright then '9' else '3')      &
                      Character'Val(48 + Color'Enum_Rep) &
                      'm');

   end Set_Foreground_Color;


   
   procedure Set_Background_Color (Color  : Color_Type;
                                   Bright : Boolean := False) is
   begin

      Ada.Text_IO.Put(ESC                                &
                      (if Bright then "4" else "10")     &
                      Character'Val(48 + Color'Enum_Rep) &
                      'm');

   end Set_Background_Color;



   procedure Set_Color (Fg_Color: Color_Type;
                        Bg_Color: Color_Type;
                        Bright  : Boolean := False) is
   begin

      Set_Foreground_Color(Fg_Color, Bright);
      Set_Background_Color(Bg_Color, Bright);

   end Set_Color;



   procedure Plain is
   begin

      Ada.Text_IO.Put(ESC & "0m");

   end Plain;

end Ansi.Colors;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
