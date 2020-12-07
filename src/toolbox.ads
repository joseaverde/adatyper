-------------------------------------------------------------------------------
--                                                                           --
--                           T O O L B O X . A D S                           --
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

--
-- @summary
-- This package contains functions common to all the project.
--
-- @description
-- Toolbox is a toolbox of functions that are used in all the project.
--
package Toolbox is

   -- The type of character we will use for this programme.
   subtype Char_Type is Wide_Character;
   
   -- The type of string we will use for this programme.
   type Str_Type is array (Positive range <>) of Char_Type;


   ----------------------
   -- TYPE CONVERSIONS --
   ----------------------

   --
   -- This procedure converts a positive number into a string, and the results
   -- are stored in a caché to speed up the execution.
   --
   -- @param Number
   -- The number we are trying to convert to a string.
   --
   -- @return
   -- The string representation of the number.
   --
   function To_String (Number: Positive)
                       return Str_Type;
   pragma Pure_Function (To_String);

   --
   -- This function converts a string type into a string.
   --
   -- @param Str
   -- The Str_Type string to convert.
   --
   -- @return
   -- The converted string.
   --
   function To_String (Str: Str_Type)
                       return String;
   pragma Inline (To_String);
   pragma Pure_Function (To_String);

   --
   -- This function converts a string into a Str_Type string.
   --
   -- @param Str
   -- The String (type) string to convert.
   --
   -- @return
   -- The converted String.
   --
   function To_Str_Type (Str: String)
                         return Str_Type;
   pragma Inline (To_Str_Type);
   pragma Pure_Function (To_Str_Type);

   --
   -- This function converts a Char_Type into a Character.
   --
   -- @param Char
   -- The character to convert.
   --
   -- @return
   -- The converted character.
   --
   function To_Character (Char: Char_Type)
                          return Character;
   pragma Inline (To_Character);
   pragma Pure_Function (To_Character);

   --
   -- This function convertes a Character into Char_Type.
   --
   -- @param Char
   -- The character to convert.
   --
   -- @return
   -- The converted character.
   function To_Char_Type (Char: Character)
                          return Char_Type;
   pragma Inline (To_Char_Type);
   pragma Pure_Function (To_Char_Type);


end Toolbox;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
