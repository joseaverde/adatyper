-------------------------------------------------------------------------------
--                                                                           --
--                      A N S I - C U R S O R S . A D S                      --
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
-- This package contains the cursor type and all its methods. The cursor
-- contains a row and column value and can be moved inside a surface or a
-- screen either virtually or phisically.
--
-- @description
-- This package contains a new type used in all the library the Cursor_Type,
-- however this is the record type, the access type is found at the top level
-- of the library (ansi.ads), so itself and other parts of the package can use
-- it without `with'ing it.
-- The cursors can be moved virtually and or phisically. Phisically means
-- moving it on the screen (the only one that should be moved this way is the
-- Main_Cursor). Virtually means only it's values.
--
package Ansi.Cursors is

   -- This is the cursor type, however the one used in this package is the one
   -- at the top level Ansi which is an access type.
   type Cursor_Type is tagged limited private;

   --------------------------------------
   -- CURSOR CREATION AND DEALLOCATION --
   --------------------------------------
   -- This part contains functions and procedures to create and free cursors.

   --
   -- This function creates a new cursor. This way you don't have to create it
   -- and then move it.
   --
   -- @param Row
   -- The row where the new cursor will be placed.
   --
   -- @param Col
   -- The column where the new cursor will be placed.
   function New_Cursor (Row: Row_Type;
                        Col: Col_Type)
                        return not null Ansi.Cursor_Type;


   --
   -- This procedure frees a cursor from memory.
   --
   -- @param Cursor
   -- The cursor to be freed.
   procedure Free (Cursor: in out Ansi.Cursor_Type);
   

   -----------------------
   -- CURSOR OPERATIONS --
   -----------------------
   -- This part contains functions and procedures to work with cursors.
   
   --
   -- This function changes the position of the cursor and returns the escape
   -- sequence to feed the stdout.
   --
   -- @param Cursor
   -- The cursor to move.
   --
   -- @param New_Row
   -- The row to place the cursor in.
   --
   -- @param New_Col
   -- The column to place the cursor in.
   --
   -- @return
   -- It returns a string type with the Ansi Escape Sequence to print to
   -- Standard Output to move the cursor. The cursor itself is also moved
   -- virtually though.
   --
   function Set_Position (Cursor : in out Cursor_Type;
                          New_Row: Row_Type;
                          New_Col: Col_Type)
                          return Str_Type;

   --
   -- This procedure changes the position of the cursor to another.
   --
   -- @param Cursor
   -- The cursor to move.
   --
   -- @param New_Row
   -- The new row.
   --
   -- @param New_Col
   -- The new column.
   --
   -- @param Move
   -- If this parameter is True, then the cursor is also moved physically.
   --
   procedure Set_Position (Cursor : in out Cursor_Type;
                           New_Row: Row_Type;
                           New_Col: Col_Type;
                           Move   : Boolean := True);

   --
   -- This procedure changes the position of the cursor to the one of another.
   --
   -- @param Cursor
   -- The cursor to move.
   --
   -- @param Cursor2
   -- The cursor whose position you want to move the first cursor to.
   --
   -- @param Move
   -- If this parameter is True, then the cursor is also moved physically.
   --
   procedure Set_Position (Cursor : in out Cursor_Type;
                           Cursor2: in     Ansi.Cursor_Type;
                           Move   : Boolean := True);

   --
   -- This procedure changes the row of the cursor.
   -- 
   -- @param Cursor
   -- The cursor to move.
   --
   -- @param New_Row
   -- The row to move it to.
   --
   -- @param Move
   -- If this parameter is True, then the cursor is also moved physically.
   --
   procedure Set_Row (Cursor : in out Cursor_Type;
                      New_Row: Row_Type;
                      Move   : Boolean := True);
   
   --
   -- This procedure changes the column of the cursor.
   --
   -- @param Cursor
   -- The cursor to move.
   --
   -- @param New_Col
   -- The column to move it to.
   --
   -- @param Move
   -- If this parameter is True, then the Cursor is also moved physically.
   --
   procedure Set_Col (Cursor : in out Cursor_Type;
                      New_Col: Col_Type;
                      Move   : Boolean := True);


   --
   -- This procedure returns the position in rows and columns of the cursor.
   --
   -- @param Cursor
   -- The cursor whose information you want to retrieve.
   --
   -- @param Out_Row
   -- The row of the Cursor.
   --
   -- @param Out_Col
   -- The column of the Cursor.
   procedure Get_Position (Cursor : in  Cursor_Type;
                           Out_Row: out Row_Type;
                           Out_Col: out Col_Type);

   --
   -- This procedure returns the row where the cursor is.
   --
   -- @param Cursor
   -- The cursor whose row position you want to retrieve.
   --
   -- @return
   -- It returns the row where the Cursor is found.
   --
   function  Get_Row (Cursor: in Cursor_Type)
                      return Row_Type;

   --
   -- This procedure returns the column where the cursor. is.
   --
   -- @param Cursor
   -- The cursor whose column position you want to retrieve.
   --
   -- @return
   -- It returns the column where the Cursor is found.
   --
   function  Get_Col (Cursor: in Cursor_Type)
                      return Col_Type;


   --
   -- This procedure moves the cursor up (by default one).
   --
   -- @param Cursor
   -- The cursor you want to move.
   --
   -- @param Rows
   -- The number of rows you want to move the cursor up.
   --
   -- @param Move
   -- If this parameter is True, then the cursor is also moved physically.
   --
   -- @exception Ansi.Exceptions.Out_Of_Bounds_Issue
   -- This exception is raised when trying to move the cursor too up.
   --
   procedure Move_Up    (Cursor: in out Cursor_Type;
                         Rows  : Positive := 1;
                         Move  : Boolean  := True);

   --
   -- This procedure moves the cursor down (by default one).
   --
   -- @param Cursor
   -- The cursor you want to move.
   --
   -- @param Rows
   -- The number of rows you want to move it down.
   --
   -- @param Move
   -- If this parameter is True, then the cursor is also moved physically.
   --
   procedure Move_Down  (Cursor: in out Cursor_Type;
                         Rows  : Positive := 1;
                         Move  : Boolean  := True);
   
   --
   -- This procedure moves the cursor to the right (by default one).
   --
   -- @param Cursor
   -- The cursor you want to move.
   --
   -- @param Cols
   -- The number of columns you want to move it to the right.
   --
   -- @param Move
   -- If this parameter is True, then the cursor is also moved physically.
   --
   procedure Move_Right (Cursor: in out Cursor_Type;
                         Cols  : Positive := 1;
                         Move  : Boolean  := True);
   
   --
   -- This procedure moves the cursor to the left (by default one).
   --
   -- @param Cursor
   -- The cursor you want to move.
   --
   -- @param Cols
   -- The number of columns you want to move it to the left.
   --
   -- @param Move
   -- If this parameter is True, then the cursor is also moved physically.
   --
   -- @exception Ansi.Exceptions.Out_Of_Bounds_Issue
   -- This exception is raised when trying to move the cursor too much to the
   -- left.
   --
   procedure Move_Left  (Cursor: in out Cursor_Type;
                         Cols  : Positive := 1;
                         Move  : Boolean  := True);

private

   --
   -- This is the cursor type definition, it's just a row and a column.
   --
   -- @field Row
   -- The row of the cursor.
   --
   -- @field Col
   -- The column of the cursor.
   --
   type Cursor_Type is tagged limited
      record
         Row: Row_Type;
         Col: Col_Type;
      end record;
   
end Ansi.Cursors;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
