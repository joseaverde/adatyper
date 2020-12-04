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
with Toolbox; use Toolbox;


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


   ----------------------
   -- STYLE OPERATIONS --
   ----------------------

   procedure Put_Style (Styles: Style_Array) is
      -- The sequence is stored in an array like this one:
      --    ESC[<>;<>;<>m
      -- Where <> are two bytes, the first one is \0 if the Style is printed,
      -- othere it's '2'. The second one is the style code.
      Size    : CONSTANT Positive := Styles'Size * 3;
      Sequence: Str_Type (1 .. Size) := (others => Char_Type'Val(0));
      Pointer : Natural := 1;
   begin
      -- We add the semicolons to the string.
      for I in Natural range 1 .. Styles'Size loop
         Sequence(3*I) := ';';
      end loop;
      Sequence(Size) := 'm';

      for S in Styles'Range loop
         Sequence(Pointer + 1) := Char_Type'Val(S'Enum_Rep + ZERO);
         if not Styles(S) then
            Sequence(Pointer) := '2';
         end if;
         Pointer := Pointer + 3;
      end loop;

      Ansi.Text_IO.Put_Ansi_Sequence(ESC & Sequence);

   end Put_Style;


   procedure Put_Style (Style: Style_Type) is
   begin

      Ansi.Text_IO.Put_Ansi_Sequence(ESC &
                                     Char_Type'Val(Style'Enum_Rep + ZERO) &
                                     'm');

   end Put_Style;



   -----------------------
   -- FORMAT OPERATIONS --
   -----------------------
   
   procedure Put_Format (Fmt: Format) is
   begin
      -- This function is only useful in non-ansi-compliant systems.
      null;

   end Put_Format;


   procedure Clear_Format is
   begin

      Ansi.Text_IO.Put_Ansi_Sequence(ESC & "[0m");

   end Clear_Format;



   -----------------------
   -- CURSOR OPERATIONS --
   -----------------------

   procedure Set_Position (Row: Row_Type;
                           Col: Col_Type) is
   begin

      Ansi.Text_IO.Put_Ansi_Sequence(ESC & To_String(Positive(Row)) &
                                     ";" & To_String(Positive(Col)) & "H");

   end Set_Position;


   function Set_Position_Ret (Row: Row_Type;
                              Col: Col_Type)
                              return Str_Type is
   begin

      return ESC & To_String(Positive(Row)) & ";" &
                   To_String(Positive(Col)) & "H";

   end Set_Position_Ret;


   procedure Move_Up (Rows: Positive := 1) is
   begin

      Ansi.Text_IO.Put_Ansi_Sequence(ESC & To_String(Rows) & "A");

   end Move_Up;

   
   procedure Move_Down (Rows: Positive := 1) is
   begin

      Ansi.Text_IO.Put_Ansi_Sequence(ESC & To_String(Rows) & "B");

   end Move_Down;


   procedure Move_Right (Cols: Positive := 1) is
   begin

      Ansi.Text_IO.Put_Ansi_Sequence(ESC & To_String(Cols) & "C");

   end Move_Right;


   procedure Move_Left (Cols: Positive := 1) is
   begin

      Ansi.Text_IO.Put_Ansi_Sequence(ESC & To_String(Cols) & "D");

   end Move_Left;

begin

   Is_Ansi_Compliant := True;

end Ansi.Compliance;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
