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

private with Ada.Interrupts;
private with Ada.Interrupts.Names;
private with Ada.Unchecked_Deallocation;
limited with Ansi.Cursors;
private with System;

--
-- @summary
-- This package contains everything used to work with Ansi Escape Code
-- Sequences. Once it is imported it sets up the terminal or the console. It
-- must be finalised manually once the program has finished.
--
-- @description
-- This package contains everything to work with ANSI escape sequences for
-- colours, styles, formatting, cursor position, surfaces.
-- It also declares some functions and procedures to work the terminal and
-- contains the main surface for the program. This way nothing is written
-- directly onto the screen but onto a surface. So this surface is protected
-- and shouldn't be written directly. Also it contains some functions to
-- initiate the terminal or console (which is executed at import time) which
-- must be finalised manually by the user, otherwise it makes the terminal
-- or the console look very ugly.
--
package Ansi is
   
   -----------
   -- TYPES --
   -----------
   -- Here we declare some types that will be used by all the child units of
   -- the ANSI package.

   -- The row type is a Positive type separated from the column type in order
   -- to avoid confusion when coding (a compiler error is raised if a wrong
   -- type is being used)
   type Row_Type is new Positive range 1 .. 2**16 - 1;
   for Row_Type'Size use 16;

   -- The column type similarly to the row type it's used for column counting
   -- I don't use the full word Column_Type because it looks better if both
   -- types have the same length.
   type Col_Type is new Positive range 1 .. 2**16 - 1;
   for Col_Type'Size use 16;

   -- This is the access type for the Ansi.Cursors.Cursor_Type which is
   -- declared in a different package for it to have primitive functions and
   -- procedures.
   type Cursor_Type is access all Ansi.Cursors.Cursor_Type;

   -- This is the surface type, it is declared here so every child package
   -- can use it even if it's private. But it's implementation is still
   -- invisible for the rest of the packages used in the programme, this
   -- type shouldn't be modified manually because it can break things and it
   -- might not work as expected.
   -- 
   -- It's used in a similar way to a sprite in graphical programmes.
   -- 
   -- @field Height
   -- The height of the created surface, it can't be changed at runtime but
   -- you can use the (very expensive) Resize function from the Ansi.Surfaces
   -- package. This function is runned everytime the terminal's or console's
   -- screen has been resized.
   -- 
   -- @field Width
   -- Simlarly to the Height parameter, this one is used to tell the number
   -- of columns the surface will have. Keep in mind in most fonts the height
   -- is the double of the width of the character. So in order to get a fully
   -- square surface you have got to set the Width doubling the Height.
   type Surface_Record (Height: Row_Type;
                        Width : Col_Type) is tagged limited private;

   -- This is the access type for the Surface_Record type, this is the type
   -- that you should be using and not the other. You can read more
   -- information about it in the Surface_Record documentation.
   type Surface_Type is access all Surface_Record;
   
   -- This type declares the avalible colours for a normal terminal or console.
   -- @value Black
   -- Black is the default background colour, keep in mind once this
   -- programme is runned the background is changed to black. It's number 0.
   -- 
   -- @value Red
   -- Red is the colour number 1.
   -- 
   -- @value Green
   -- Green is the best colour, whose number is 2.
   -- 
   -- @value Yellow
   -- Yellow might look like Orange if not bold or bright in some terminals.
   -- It's number 3.
   -- 
   -- @value Blue
   -- Blue is number 4.
   -- 
   -- @value Magenta
   -- Magenta (a.k.a Pink) is number 5.
   -- 
   -- @value Cyan
   -- Cyan is a lighter Blue colour (number 6).
   -- 
   -- @value White
   -- White is the default colour for foreground. It's the last one
   -- (number 7).
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
   --
   -- @value Bright
   -- Bright is the same as a Bold character. This can also be done if the
   -- Bright parameter for any colour function is set to true.
   --
   -- @value Dim
   -- Dim is a very soft colour without many light, well DIM.
   --
   -- @value Italics
   -- Ciao!
   --
   -- @value Underlined
   -- Draw a line under it.
   --
   -- @value Slow_Blink
   -- It's just blinking.
   --
   -- @value Reversed
   -- Inside out! Change background for foreground colour.
   type Style_Type is (Bright, Dim, Italics, Underlined, Slow_Blink, Reversed);
   for Style_Type use
      (Bright     => 1,
       Dim        => 2,
       Italics    => 3,
       Underlined => 4,
       Slow_Blink => 5,
       Reversed   => 7);
   for Style_Type'Size use 3;

   -- This type is just an array of styles telling which are on and which off.
   type Style_Array is array (Style_Type'Range) of Boolean
      with Default_Component_Value => False;
   pragma Pack(Style_Array);

   -- The type of character we will use for this programme.
   subtype Char_Type is Wide_Character;
   
   -- The type of string we will use for this programme.
   type Str_Type is array (Positive range <>) of Char_Type;

   
   
   ---------------
   -- CONSTANTS --
   ---------------

   -- This constant contains the first part of every escape code sequence.
   ESC: CONSTANT Str_Type (1 .. 2) :=
      (1 => Char_Type'Val(Character'Pos(ASCII.ESC)),
       2 => Char_Type'Val(Character'Pos('[')));

   -- This constant is a shortcut for True when printing a colour bright.
   Is_Bright: CONSTANT Boolean := True;

   -- This is not a constant but an access type. It's the cursor of the screen.
   -- It's the one that is moved directly.
   Main_Cursor: Cursor_Type;
   

   ----------------------------
   -- SURFACE_TYPE FUNCTIONS --
   ----------------------------

   --
   -- This function changes the position of a surface.
   --
   -- @param Surface
   -- The surface we are moving.
   -- 
   -- @param Row
   -- The row where we are moving it.
   --
   -- @param Col
   -- The column where we are moving it.
   --
   -- @exception Ansi.Exceptions.Invalid_Surface_Issue
   -- This exception is raised when the surface is protected (like the main
   -- surface) which shouldn't be moved manually, it must fit the terminal's or
   -- the console's screen.
   --
   -- @exception Ansi.Exceptions.Using_Null_Surface_Issue
   -- This exception is raised if trying to change the position of null or a
   -- freed surface.
   --
   procedure Set_Position (Surface: Surface_Type;
                           Row    : Row_Type;
                           Col    : Col_Type);


   ---------------
   -- FUNCTIONS --
   ---------------
   
   -- This procedure clears the screen.
   procedure Clear;

   --
   -- This function returns the height of the screen.
   --
   -- @return
   -- The Height of the current terminal or console (it isn't updated, it just
   -- returns the last one).
   --
   function Get_Height return Row_Type;
   pragma Inline (Get_Height);
   
   --
   -- This function returns the width of the screen.
   --
   -- @return
   -- The Width of the current terminal or console (it isn't updated, though.
   -- It just returns the last width).
   --
   function Get_Width  return Col_Type;
   pragma Inline (Get_Width);

   --
   -- This function returns the main surface.
   --
   -- @return
   -- The Main_Surface, the surface that takes all the terminal or the console.
   --
   function Get_Main_Surface return Surface_Type;
   pragma Inline (Get_Main_Surface);
   
   -- This procedure updates the main surface if it has been resized.
   procedure Update_Main_Surface;

   -- This procedure restores the old terminal.
   procedure Finalize;

 
private
   
   -----------
   -- TYPES --
   -----------
   -- These private types are used mainly by the surface type.   

   --
   -- The format type contains the information about formatting in every cell
   -- of the matrix using only two bytes of memory to store the colour, the
   -- brightness and the style.
   --
   -- @field Fg_Color
   -- The colour in the foreground.
   --
   -- @field Fg_Bright
   -- The brightness of the foreground colour.
   --
   -- @field Bg_Color
   -- The colour in the background.
   --
   -- @field Bg_Bright
   -- The brightness of the background colour.
   --
   -- @field Style
   -- An array of styles that can be set at once on a character.
   --
   type Format is
      record
         Fg_Color  : Color_Type;
         Fg_Bright : Boolean;
         Bg_Color  : Color_Type;
         Bg_Bright : Boolean;
         Style     : Style_Array;
      end record;
   -- We pack it even if it costs more.
   -- XXX: Check whether it's more efficient to leave it without setting the
   -- bytes in memory.
   for Format use
      record
         Fg_Color  at 0 range 0 .. 2;
         Fg_Bright at 0 range 3 .. 3;
         Bg_Color  at 0 range 4 .. 6;
         Bg_Bright at 0 range 7 .. 7;
         Style     at 1 range 0 .. Style_Array'Length - 1;
      end record;
   for Format'Size use 2 * System.Storage_Unit;

   --
   -- Each of the elements of the Surface_Record array.
   --
   -- @field Fmt
   -- The format of every element of the matrix.
   --
   -- @field Char
   -- The Character the cell contains.
   --
   type Element is
      record
         Fmt  : Format;
         Char : Char_Type;
      end record;
   -- We pack it up.
   for Element use
      record
         Fmt   at 0 range 0 .. 15;
         Char  at 2 range 0 .. Char_Type'Size - 1;
      end record;
   for Element'Size use 4 * System.Storage_Unit;

   -- Finally we declare the matrix (the grid) that will contain the
   -- information about the Surface.
   type Matrix is array (Row_Type range <>, Col_Type range <>) of Element;
   
   -- TODO: Add pools
   -- This is the queue used to store the performed operations onto a surface,
   -- it must be updated to free it.
   --
   type Operation_Record;

   --
   -- This is the access type to the operation_record type.
   --
   type Operation is access Operation_Record;

   --
   -- This is the queue used to store the performed operations onto a surface,
   -- it must be updated to free it.
   --
   -- @field Next
   -- The next operation in the queue
   --
   -- @field Cursor
   -- The cursor, the position of the queue.
   --
   type Operation_Record is
      record
         Next  : Operation := null;
         Cursor: Cursor_Type;
      end record;

   -- This procedure frees an operation access type.
   procedure Free is new Ada.Unchecked_Deallocation(Object => Operation_Record,
                                                    Name   => Operation);

   --
   -- This is the declaration for the Surface type.
   -- It will be a matrix of Wide_Characters and Formats (Elements).
   --
   -- @field Height
   -- The height of the new surface.
   --
   -- @field Width
   -- The width of the new surface.
   --
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
                                   Char => Char_Type'Val(Character'Pos(' ')))));


         -- Every operation performed onto the screen is stored onto a queue,
         -- so it requires the minimum number of operations and changes when
         -- updating.
         
         -- The Head is a fixed pointer to the first element of the queue.
         Head: Operation := Null;

         -- The Tail is a moving pointer where the last element of the queue is
         -- added.
         Tail: Operation := Null;

         -- The grid also has god a position in the main screen, the
         -- coordinates are Y and X not X and Y (because it's counted in rows
         -- and columns).

         -- The Y position (The row).
         Row: Row_Type := 1;

         -- The X position (The column).
         Col: Col_Type := 1;

         -- Finally we add a variable to tell whether the surface has to be
         -- completely updated or not.
         Update_All: Boolean := True;

         -- The current cursor position in this surface.
         Cursor    : Cursor_Type;

         -- The default format the cursor will write in.
         Cursor_Fmt: Format := Format'(Fg_Color  => White,
                                       Fg_Bright => False,
                                       Bg_Color  => Black,
                                       Bg_Bright => False,
                                       Style     => (others => False));

         -- If this is set to true this surface can't be used as a layer or
         -- under certain circunstances. An error would be raised instead.
         Protect_It: Boolean := False;
      end record;

   
   --
   -- This procedure pushes a new row and column into the operation queue of a
   -- surface.
   --
   -- @param Surface
   -- The surface where to push the new Row and Column.
   --
   -- @param Cursor
   -- The pointer to a cursor, it's not copied, it's given as it is to the
   -- stack, so it might get freed if you haven't have copied it.
   --
   procedure Push (Surface: in out Surface_Record;
                   Cursor :        Cursor_Type);


   -----------------
   -- IDENTIFIERS --
   -----------------

   -- The main surface.
   Main_Surface: Surface_Type := new Surface_Record(1, 1);

   -- The height of the screen.
   Height: Row_Type := 1;

   -- The width of the screen.
   Width : Col_Type := 1;

   -- This variable is true, when the screen has been resized.
   Has_Resized: Boolean;


   -------------
   -- SIGNALS --
   -------------
   -- This part contains the signal handlers in the package.
   
   --
   -- This protected `something' contains functions that occur when a specific
   -- signal or interruption is given. The Ansi.Text_IO package is called and
   -- given the signal to give it to the user.
   --
   protected Event_Handler is
      -- 
      -- This procedure updates the terminal size. This function uses special
      -- functions depending on the operating system. It also resizes the main
      -- surface. This function is raised automatically every time a signal
      -- SIGWINCH is raised.
      --
      procedure Update_Terminal_Size;
      pragma Interrupt_Handler (Update_Terminal_Size);
      pragma Attach_Handler (Update_Terminal_Size,
                             Ada.Interrupts.Names.SIGWINCH);
   end Event_Handler;

end Ansi;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
