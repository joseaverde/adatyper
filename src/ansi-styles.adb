-------------------------------------------------------------------------------
--                                                                           --
--                       A N S I - S T Y L E S . A D B                       --
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


package body Ansi.Styles is


   procedure Set_Style (Style: Style_Type) is
   begin

      Ada.Text_IO.Put(ESC & Character'Val(48 + Style'Enum_Rep) & 'm');
      Styles_Used(Style) := True;

   end Set_Style;


   
   procedure Remove_Style (Style: Style_Type) is
   begin

      Ada.Text_IO.Put(ESC & '2' & Character'Val(48 + Style'Enum_Rep) & 'm');
      Styles_Used(Style) := False;

   end Remove_Style;


   
   procedure Remove_All_Styles is
   begin

      for Style in Styles_Used'Range loop
         if Styles_Used(Style) then
            Remove_Style(Style);
         end if;
      end loop;

   end Remove_All_Styles;



   procedure Plain is
   begin

      Ada.Text_IO.Put(ESC & "0m");

   end Plain;

end Ansi.Styles;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
