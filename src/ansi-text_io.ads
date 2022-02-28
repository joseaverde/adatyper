-------------------------------------------------------------------------------
--                                                                           --
--                      A N S I - T E X T _ I O . A D S                      --
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
-- This package contains input and output functions.
--
-- @description
-- Instead of using the normal Ada.Wide_Text_IO package, I'm using this to add
-- a very thin indirect layer to the package. So if I decide to go full Wide
-- Wide, I have to change as minimum as possible.
package Ansi.Text_IO is
   
   -- This procedure flushes the Standard Output.
   procedure Flush;
   pragma Inline (Flush);

   -- This procedure prints a character onto the screen.
   procedure Put (Item: Char_Type);
   
   -- This procedure prints a character onto the screen.
   procedure Put (Item: Str_Type);

   -- This procedure prints an Ansi Sequence without moving the cursor.
   procedure Put_Ansi_Sequence (Item: Str_Type);
   
   
   -- The special keys are the arrow keys...
   type Special_Key_Code is (ARROW_UP, ARROW_DOWN, ARROW_LEFT, ARROW_RIGHT);

   -- The key kind is the kind of key that has been pressed.
   type Key_Kind is (Ordinary, Special, Long);

   -- This type is used to return which key has been pressed.
   type Key_Type is tagged private;

   -- This function waits for the user to write its input which is stored in
   -- both the surface in the returned variable. If it's null, it's not written
   function Get_Input (Surface: Surface_Type := null)
                       return Str_Type;

   -- This function returns whether a new input key has been pressed, if so the
   -- Key parameter is changed.
   function Get_Key (Key: out Key_Type)
                     return Boolean;

   -- Returns the character value of a key, if it wasn't a character key, an
   -- error is raised.
   function Get_Char (Key: in Key_Type)
                      return Char_Type;
   pragma Inline (Get_Char);

   -- Returns the special key code of a pressed ley, if it wasn't a character
   -- key an error is raised.
   function Get_Code (Key: in Key_Type)
                      return Special_Key_Code;
   pragma Inline (Get_Code);

   -- Returns the kind of key a key is.
   function Get_Kind (Key: in Key_Type)
                      return Key_Kind;
   pragma Inline (Get_Kind);

private

   type Key_Type is tagged
      record
         Char: Char_Type;
         Code: Special_Key_Code;
         Kind: Key_Kind;
      end record;


end Ansi.Text_IO;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
