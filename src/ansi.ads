-------------------------------------------------------------------------------
--                                                                           --
--                              A N S I . A D S                              --
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

private with Ada.Unchecked_Deallocation;
limited with Ansi.Cursors;
private with Interfaces.C;
private with System;


-- This package contains everything to work with ANSI escape sequences for
-- colours, styles, formatting, cursor position...
-- It also declares some functions and procedures to work the terminal and it
-- contains the main surface for the program. This way nothing is written
-- directly onto the screen but onto a surface.
package Ansi is
   

   -----------
   -- TYPES --
   -----------

   type Row_Type is new Positive;
   type Col_Type is new Positive;
   type Cursor_Type is access all Ansi.Cursors.Cursor_Type;

   -- This is the surface type, it is declared here so every child package can
   -- use even if it's private and it's implementation still being invisible
   -- for the rest of the program.
   type Surface_Record (Height: Row_Type;
                        Width : Col_Type) is tagged limited private;
   type Surface_Access is access all Surface_Record;
   
   -- This type declares the avalible colours for a normal terminal or console.
   -- FIXME: It may need some changes for the Windows console.
   type Color_Type is (Black, Red, Green, Yellow, Blue, Magenta, Cyan, White);
   for Color_Type use
      (Black   => 0,
       Red     => 1,
       Green   => 2,
       Yellow  => 3,
       Blue    => 4,
       Magenta => 5,
       Cyan    => 6,
       White   => 7);
   for Color_Type'Size use 3;

   -- This type is used to describe the different kind of styles you can add to
   -- a terminal or console.
   type Style_Type is (Bright, Dim, Italics, Underlined, Reversed);
   for Style_Type use
      (Bright     => 1,
       Dim        => 2,
       Italics    => 3,
       Underlined => 4,
       Reversed   => 7);
   for Style_Type'Size use 3;

   -- The type of character we will use for this program.
   subtype Char_Type is Wide_Character;
   
   -- The type of string we will use for this program.
   type Str_Type is array (Positive range <>) of Char_Type;

    
   
   ---------------
   -- CONSTANTS --
   ---------------

   -- This constant contains the first part of every escape code sequence.
   ESC: CONSTANT Str_Type (1 .. 2) :=
      (1 => Char_Type'Val(Character'Pos(ASCII.ESC)),
       2 => Char_Type'Val(Character'Pos('[')));

   -- This constant is a shortcut for True when setting a colour.
   Is_Bright: CONSTANT Boolean := True;

   -- This is not a constant but an access type.
   Main_Cursor: Cursor_Type;
   

   ---------------
   -- FUNCTIONS --
   ---------------

   -- This function returns the height of the screen.
   function Get_Height return Row_Type;
   pragma Inline (Get_Height);
   
   -- This function returns the width of the screen.
   function Get_Width  return Col_Type;
   pragma Inline (Get_Width);

   -- This function returns the main surface.
   function Get_Main_Surface return Surface_Access;
   pragma Inline (Get_Main_Surface);
   
   -- This procedure updates the main surface if it has been resized.
   procedure Update_Main_Surface;

   -- This function is a wrapper for the Ioctl function. It returns True if
   -- either the width or the height or both have changed.
   function Update_Terminal_Size return Boolean;
   pragma Inline (Update_Terminal_Size);

 
-------------------------------------------------------------------------------
private -----------------------------------------------------------------------
-------------------------------------------------------------------------------
   
   -----------
   -- IOCTL --
   -----------

   -- This is the winsize type from sys/ioctl.h library from C. It's used to
   -- get the terminal size.
   type Winsize is
      record
         ws_row   : Interfaces.C.unsigned_short;
         ws_col   : Interfaces.C.unsigned_short;
         ws_xpixel: Interfaces.C.unsigned_short;
         ws_ypixel: Interfaces.C.unsigned_short;
      end record
         with Convention => C;

   -- This function gets the size of the screen.
   function Ioctl (Fd     :     Interfaces.C.int;
                   Request:     Interfaces.C.unsigned_long;
                   Struct : out Winsize)
                   return Interfaces.C.int;
   pragma Import (C, Ioctl, "ioctl");

   -- This constant is found under /usr/include/asm-generic/ioctls.h and is the
   -- request to get the size of the window with Ioctl, its a MACRO so it can't
   -- be imported natively, so I'm writting the value found in the file.
   TIOCGWINSZ: CONSTANT Interfaces.C.unsigned_long := 16#5413#;
   

   -----------
   -- TYPES --
   -----------
  
   -- This type is just an array of styles telling which are on and which off.
   type Style_Array is array (Style_Type'Range) of Boolean
      with Default_Component_Value => False;
   pragma Pack(Style_Array);
   
   -- The format type contains the information about formatting in every cell
   -- of the matrix using only two bytes of memory to store the colour, the
   -- brightness and the style.
   type Format is
      record
         Fg_Color  : Color_Type;
         Fg_Bright : Boolean;
         Bg_Color  : Color_Type;
         Bg_Bright : Boolean;
         Style     : Style_Array;
      end record;
   for Format use
      record
         Fg_Color  at 0 range 0 .. 2;
         Fg_Bright at 0 range 3 .. 3;
         Bg_Color  at 0 range 4 .. 6;
         Bg_Bright at 0 range 7 .. 7;
         Style     at 1 range 0 .. 4;
      end record;
   for Format'Size use 2 * System.Storage_Unit;
   pragma Pack (Format);

   -- Each of the elements of the Surface_Record array.
   type Element is
      record
         Fmt  : Format;
         -- The Wide_Character may become a Wide_Wide_Character in the future
         -- if more languages are added, but for now we will stick to a 2 bytes
         -- character.
         Char : Char_Type;
      end record;
   for Element use
      record
         Fmt   at 0 range 0 .. 15;
         Char  at 2 range 0 .. Char_Type'Size - 1;
      end record;
   for Element'Size use 4 * System.Storage_Unit;
   pragma Pack (Element);

   -- Finally we declare the matrix (the grid) that will contain the
   -- information about the Surface.
   type Matrix is array (Row_Type range <>, Col_Type range <>) of Element;
   
   -- This is the tail used to store the performed operations onto a surface,
   -- it must be updated to free it.
   type Operation_Record;
   type Operation is access Operation_Record;
   type Operation_Record is
      record
         Next  : Operation := null;
         Cursor: Cursor_Type;
      end record;

   -- This procedure frees an operation.
   procedure Free is new Ada.Unchecked_Deallocation(Object => Operation_Record,
                                                    Name   => Operation);

   -- This is the declaration for the Surface type.
   -- It will be a matrix of Wide_Characters and Escape sequences.
   type Surface_Record (Height: Row_Type; Width: Col_Type) is tagged limited
      record
         -- The grid is a matrix that contains all the elements of the surface.
         Grid: Matrix (1 .. Height, 1 .. Width) :=
            (others => 
               (others => Element'(Fmt  => Format'(Fg_Color  => White,
                                                   Fg_Bright => False,
                                                   Bg_Color  => Black,
                                                   Bg_Bright => False,
                                                   Style     =>
                                                      (others => False)),
                                   Char => Char_Type'Val(0))));

         -- Every operation performed onto the screen is stored onto a tail, so
         -- it requires the minimum number of operations and changes when
         -- updating.
         Head: Operation := Null;
         Tail: Operation := Null;

         -- The grid also has god a position in the main screen, the
         -- coordinates are Y and X not X and Y (because it's counted in rows
         -- and columns).
         Row, Column: Positive := 1;

         -- Finally we add a variable to tell whether the surface has to be
         -- completely updated or not.
         Uptade_All: Boolean := True;

         -- The current cursor position in this surface.
         Cursor    : Cursor_Type;
      end record;

   
   -- This procedure pushes a new row and column into the operation tail of a
   -- surface.
   procedure Push (Surface: in out Surface_Record;
                   Cursor :        Cursor_Type);

   -----------------
   -- IDENTIFIERS --
   -----------------

   -- The main surface.
   Main_Surface: Surface_Access;

   -- The dimensions of the screen.
   Height: Row_Type := 1;
   Width : Col_Type := 1;

   
end Ansi;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
