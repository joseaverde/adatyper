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

   type WORD is new Interfaces.Unsigned_16;

   Last_Format: Format := Format'(Fg_Color  => White,
                                  Fg_Bright => False,
                                  Bg_Color  => Black,
                                  Bg_Bright => False,
                                  Style     => (others => False));

   ---------------
   -- CONSTANTS --
   ---------------

   ATTRIBUTE_ZERO          : CONSTANT := 16#0000#;
   FOREGROUND_INTENSITY    : CONSTANT := 16#0008#;
   BACKGROUND_INTENSITY    : CONSTANT := 16#0080#;
   COMMON_LVB_REVERSE_VIDEO: CONSTANT := 16#4000#;
   COMMON_LVB_UNDERSCORE   : CONSTANT := 16#8000#;

   Windows_Conversion: CONSTANT array (Color_Type'Range) of WORD :=
      (Black   => 2#0000#,
       Red     => 2#0100#,
       Green   => 2#0010#,
       Yellow  => 2#0110#,
       Blue    => 2#0001#,
       Magenta => 2#0101#,
       Cyan    => 2#0011#,
       White   => 2#0111#);


   -----------------------
   -- COLORS OPERATIONS --
   -----------------------

   -- This procedure sets the console's colour and attributes (styles)
   procedure Set_Windows_Console_Color_With_Attributes is
      Color: CONSTANT WORD :=
         Windows_Conversion(Last_Format.Fg_Color) +
         Windows_Conversion(Last_Format.Bg_Color) * 16 +
         (if Last_Format.Style(Bright) or Last_Format.Fg_Bright then
                                        FOREGROUND_INTENSITY
          else                          ATTRIBUTE_ZERO) +
         (if Last_Format.Bg_Bright then BACKGROUND_INTENSITY
          else                          ATTRIBUTE_ZERO) +
         (if Last_Format.Style(Underlined) then COMMON_LVB_UNDERSCORE
          else                                  ATTRIBUTE_ZERO) +
         (if Last_Format.Style(Reversed)   then COMMON_LVB_REVERSE_VIDEO
          else                                  ATTRIBUTE_ZERO);

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


   -----------------------
   -- FORMAT OPERATIONS --
   -----------------------

   procedure Put_Format (Fmt: Format) is
   begin

      if Last_Format /= Fmt then
         Last_Format := Fmt;
         Set_Windows_Console_Color_With_Attributes;
      end if;

   end Put_Format;


begin

   Is_Ansi_Compliant := False;

end Ansi.Compliance;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
