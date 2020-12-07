-------------------------------------------------------------------------------
--                                                                           --
--                           T O O L B O X . A D B                           --
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

with Ada.Characters.Conversions;


package body Toolbox is

   function To_String (Number: Positive)
                       return Str_Type is
      Size: Natural := 1;
   begin
      
      while Number / 10 ** Size /= 0 loop
         Size := Size + 1;
      end loop;

      Create_String:
         declare
            Str: Str_Type (1 .. Size);
         begin
            for I in Natural range 1 .. Size loop
               Str(Size + 1 - I) := Char_Type'Val(
                                       (Number / 10**(I-1)) mod 10 + 48);
            end loop;

            return Str;
         end Create_String;
               
   end To_String;


   function To_String (Str: Str_Type)
                       return String is
      Wide_Str: Wide_String(Str'Range);
   begin
      
      for I in Wide_Str'Range loop
         Wide_Str(I) := Str(I);
      end loop;

      return Ada.Characters.Conversions.To_String(Wide_Str);

   end To_String;


   function To_Str_Type (Str: String)
                         return Str_Type is
      Wide_Str: constant Wide_String := Ada.Characters.Conversions.
                                             To_Wide_String(Str);
      Str_Type_Str: Str_Type(Wide_Str'Range);
   begin

      for I in Str_Type_Str'Range loop
         Str_Type_Str(I) := Wide_Str(I);
      end loop;

      return Str_Type_Str;

   end To_Str_Type;


   function To_Character (Char: Char_Type)
                          return Character is
   begin

      return Ada.Characters.Conversions.To_Character(Char);

   end To_Character;


   function To_Char_Type (Char: Character)
                          return Char_Type is
   begin

      return Ada.Characters.Conversions.To_Wide_Character(Char);

   end To_Char_Type;

end Toolbox;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
