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

end Toolbox;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
