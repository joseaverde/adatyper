-------------------------------------------------------------------------------
--                                                                           --
--                          E N C O D I N G . A D B                          --
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

package body Encoding is

   function Convert (Source: Str_Type)
                     return String is
      Str: String(1 .. Source'Length * 2);
      Len: Natural := 0;
      N  : Natural;
   begin
      
      for I in Source'Range loop
         N := Char_Type'Pos(Source(I));
         if N < 256 then
            Len := Len + 1;
            Str(Len) := Character'Val(N);
         else
            Len := Len + 2;
            Str(Len - 1) := Character'Val(N / 256);
            Str(Len) := Character'Val(N mod 256);
         end if;
      end loop;

      return Str(1 .. Len);

   end Convert;


   function Convert (Source: Char_Type)
                     return String is
      N: constant Natural := Char_Type'Pos(Source);
   begin

      if N < 256 then
         return Character'Val(N) & "";
      else
         return Character'Val(N / 256) & Character'Val(N mod 256);
      end if;

   end Convert;
      
end Encoding;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
