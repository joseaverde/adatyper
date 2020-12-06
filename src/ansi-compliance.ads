-------------------------------------------------------------------------------
--                                                                           --
--                   A N S I - C O M P L I A N C E . A D S                   --
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
-- This package is used to define some common functions that depend on whether
-- the terminal/console is ansi-compliant or not.
--
-- @description
-- Instead of having millions of external implementation files for each of the
-- functions and procedures that differ depending on whether the terminal or
-- the console is ansi-compliant or not, we are going to create this private
-- package to handle does parts with conditional compilation. Keep in mind even
-- though the newest versions of Windows10 allow Ansi Escape Sequences, older
-- versions doesn't, that's why I'm separing it (for portability). Also even if
-- Windows isn't POSIX compliant, it will use the implementation found in the
-- `%/src/posix/ansi-compliant' directory. For now there are no implementation
-- for POSIX-compliant non-ANSI-compliant operating systems (I don't even know
-- if they exist)
--
private package Ansi.Compliance is

   pragma Elaborate_Body (Ansi.Compliance);

   ------------------------
   -- COLOURS OPERATIONS --
   ------------------------
   
   --
   -- This function depends on the compliance. If it is ansi-compliant it will
   -- generate the number that must be written between `ESC[' and `m' to put
   -- that same colour in an ansi-compliant terminal. In Windows it will return
   -- a null string `""' and call the function to change it.
   --
   -- @param Color
   -- The colour we want to get the foreground colour sequence from.
   --
   -- @param Bright
   -- The brightness of such colour.
   --
   -- @return
   -- In Windows it returns "" and in others it returns the code to change that
   -- colour without the `ESC[ m' as explained in the description.
   --
   function Gen_Foreground (Color : Color_Type;
                            Bright: Boolean)
                            return Str_Type;
   pragma Inline (Gen_Foreground);
   pragma Pure_Function (Gen_Foreground);

   --
   -- This function depends on the compliance. If it is ansi-compliant it will
   -- generate the number that must be written between `ESC[' and `m' to put
   -- that same colour in an ansi-compliant terminal. In Windows it will return
   -- a null string `""' and call the function to change it.
   --
   -- @param Color
   -- The colour we want to get the background colour sequence from.
   --
   -- @param Bright
   -- The brightness of such colour.
   --
   -- @return
   -- In Windows it returns "" and in others it returns the code to change that
   -- colour without the `ESC[ m' thing as explained in the description.
   --
   function Gen_Background (Color : Color_Type;
                            Bright: Boolean)
                            return Str_Type;
   pragma Inline (Gen_Background);
   pragma Pure_Function (Gen_Background);


   --
   -- This procedure puts a colour into standard output's foreground.
   --
   -- @param Color
   -- The colour to put.
   --
   -- @param Bright
   -- The brightness of such colour.
   --
   procedure Put_Foreground (Color : Color_Type;
                             Bright: Boolean);
   pragma Inline (Put_Foreground);

   --
   -- This procedure puts a colour into standard output's background.
   --
   -- @param Color
   -- The colour to put.
   --
   -- @param Bright
   -- The brightness of such colour.
   --
   procedure Put_Background (Color : Color_Type;
                             Bright: Boolean);
   pragma Inline (Put_Background);


   ----------------------
   -- STYLE OPERATIONS --
   ----------------------
   -- This part declares the styling procedures.

   --
   -- This function returns the code to put a style in standard output without
   -- the trailing `ESC[' and the `m'.
   --
   -- @param Style
   -- The style to generate.
   --
   -- @param Remove
   -- If this parameter is set to true, it will return the code to remove the
   -- given style.
   --
   -- @return
   -- The code to put the given style, in Windows it returns a null string,
   -- though.
   --
   function Gen_Style (Style : Style_Type;
                       Remove: Boolean := False)
                       return Str_Type;
   pragma Inline (Gen_Style);
   pragma Pure_Function (Gen_Style);
   
   --
   -- This procedure is used to set an array of styles into standard output.
   -- However in Windows most of the styles are not available.
   --
   -- @param Styles
   -- The styles to put into standard output.
   --
   procedure Put_Style (Styles: Style_Array);
   pragma Inline (Put_Style);

   --
   -- This procedure is used to put a single style into standard output.
   -- However most of the styles available under ansi-compliant terminals are
   -- not available in the Windows console (CMD).
   -- 
   -- @param Style
   -- The style to put into standard output.
   --
   procedure Put_Style (Style: Style_Type);
   pragma Inline (Put_Style);
   

   -----------------------
   -- FORMAT OPERATIONS --
   -----------------------

   --
   -- This procedure puts a full format into the console, the format is a
   -- private type only available in this package's private part.
   --
   -- @param Fmt
   -- The format.
   --
   procedure Put_Format (Fmt: Format);
   pragma Inline (Put_Format);

   --
   -- This procedure clears the format
   --
   procedure Clear_Format;
   pragma Inline (Clear_Format);

   
   -----------------------
   -- CURSOR OPERATIONS --
   -----------------------
   -- This part is used to move the cursor depending on the operating system
   -- and ansi compliance.

   --
   -- This procedure sets the position of the cursor on the screen.
   --
   -- @param Row
   -- The row to move the cursor to.
   --
   -- @param Col
   -- The column to move the cursor to.
   --
   procedure Set_Position (Row: Row_Type;
                           Col: Col_Type);
   pragma Inline (Set_Position);

   --
   -- This function returns the escape sequence to use to move the cursor up,
   -- however, in non-ansi-compliant Windows systems it will also change it and
   -- return a null string.
   --
   -- @param Row
   -- The row to move the cursor to.
   --
   -- @param Col
   -- The column to move the cursor to.
   --
   -- @return
   -- - In non-ansi-compliant windows systems it returns a null string.
   -- - In ansi-compliant systems it returns the ansi escape sequence.
   --
   function Set_Position_Ret (Row: Row_Type;
                              Col: Col_Type)
                              return Str_Type;
   pragma Pure_Function (Set_Position_Ret);
   pragma Inline (Set_Position_Ret);

   --
   -- This procedure moves the position of the cursor up. In windows it uses
   -- the Set_Position function if it's non-ansi-compliant.
   --
   -- @param Rows
   -- The number of rows to move up.
   --
   procedure Move_Up (Rows: Positive := 1);
   pragma Inline (Move_Up);

   --
   -- This procedure moves the position of the cursor up. In windows it uses
   -- the Set_Position function if it's non-ansi-compliant.
   --
   -- @param Rows
   -- The number of rows to move up.
   --
   procedure Move_Down (Rows: Positive := 1);
   pragma Inline (Move_Down);

   --
   -- This procedure moves the position of the cursor up. In windows it uses
   -- the Set_Position function if it's non-ansi-compliant.
   --
   -- @param Cols
   -- The number of rows to move up.
   --
   procedure Move_Right (Cols: Positive := 1);
   pragma Inline (Move_Right);
   
   --
   -- This procedure moves the position of the cursor up. In windows it uses
   -- the Set_Position function if it's non-ansi-compliant.
   --
   -- @param Cols
   -- The number of rows to move up.
   --
   procedure Move_Left (Cols: Positive := 1);
   pragma Inline (Move_Left);
   

   -------------
   -- GLOBALS --
   -------------

   -- This variable tells whether the terminal or the console is ansi-compliant
   Is_Ansi_Compliant: Boolean;

end Ansi.Compliance;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
