-------------------------------------------------------------------------------
--                                                                           --
--                   A N S I - C O M P L I A N C E . A D B                   --
--                        A N S I - C O M P L I A N T                        --
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

with Ansi.Text_IO;


package body Ansi.Compliance is


   ZERO: CONSTANT Natural := Character'Pos('0');

   -----------------------
   -- COLOUR OPERATIONS --
   -----------------------

   function Gen_Foreground (Color : Color_Type;
                            Bright: Boolean)
                            return Str_Type is
   begin

      return (if Bright then
                  '9'
              else
                  '3'
              ) & Char_Type'Val(ZERO + Color'Enum_Rep);

   end Gen_Foreground;


   function Gen_Background (Color : Color_Type;
                            Bright: Boolean)
                            return Str_Type is
   begin

      return (if Bright then
                  "10"
              else
                  "4") & Char_Type'Val(ZERO + Color'Enum_Rep);

   end Gen_Background;



   procedure Put_Foreground (Color : Color_Type;
                             Bright: Boolean) is
   begin

      Ansi.Text_IO.Put_Ansi_Sequence(ESC & Gen_Foreground(Color,Bright) & "m");

   end Put_Foreground;

   
   procedure Put_Background (Color : Color_Type;
                             Bright: Boolean) is
   begin

      Ansi.Text_IO.Put_Ansi_Sequence(ESC & Gen_Background(Color,Bright) & "m");

   end Put_Background;

end Ansi.Compliance;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
