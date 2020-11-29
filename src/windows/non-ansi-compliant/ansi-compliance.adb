-------------------------------------------------------------------------------
--                                                                           --
--                   A N S I . C O M P L I A N C E . A D B                   --
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

with Interfaces;


package body Ansi.Compliance is

   -- TODO: Is_Ansi_Compliant := False

   Last_Format: Format := Format'(Fg_Color  => White,
                                  Fg_Bright => False,
                                  Bg_Color  => Black,
                                  Bg_Bright => False,
                                  Style     => (others => False));

   -- This procedure sets the console's colour and attributes (styles)
   procedure Set_Windows_Console_Color_With_Attributes is
      type WORD is new Interfaces.Unsigned_16;
      Color: CONSTANT WORD := 1;
      -- TODO Finish

      procedure C_Driver_Set_Windows_Console_Color_With_Attributes(C: WORD);
      pragma Import (C, C_Driver_Set_Windows_Console_Color_With_Attributes,
                     "setWindowsConsoleColorWithAttributes");
   begin
 
      C_Driver_Set_Windows_Console_Color_With_Attributes(Color);

   end Set_Windows_Console_Color_With_Attributes;


   
   function Gen_Foreground (Color : Color_Type;
                            Bright: Boolean)
                            return Str_Type is
   begin

      Last_Format.Fg_Color  := Color;
      Last_Format.Fg_Bright := Bright;

      Set_Windows_Console_Color_With_Attributes;

      return "";

   end Gen_Foreground;


   function Gen_Background (Color : Color_Type;
                            Bright: Boolean)
                            return Str_Type is
   begin

      Last_Format.Bg_Color  := Color;
      Last_Format.Bg_Bright := Bright;

      Set_Windows_Console_Color_With_Attributes;

      return "";

   end Gen_Background;



   procedure Put_Foreground (Color : Color_Type;
                             Bright: Boolean) is
   begin

      Last_Format.Fg_Color  := Color;
      Last_Format.Fg_Bright := Bright;

      Set_Windows_Console_Color_With_Attributes;

   end Put_Foreground;


   procedure Put_Background (Color : Color_Type;
                             Bright: Boolean) is
   begin

      Last_Format.Bg_Color  := Color;
      Last_Format.Bg_Bright := Bright;

      Set_Windows_Console_Color_With_Attributes;

   end Put_Background;

end Ansi.Compliance;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
