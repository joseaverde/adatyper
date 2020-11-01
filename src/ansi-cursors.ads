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

-- This package contains the cursor type and all its methods. The cursor can be
-- moved in the screen (and always virtually) if the Move parameter is True.
package Ansi.Cursors is

   type Cursor_Type is tagged limited private;
   
   -- This procedure changes the position of the cursor to another.
   procedure Set_Position (Cursor : in out Cursor_Type;
                           New_Row: Row_Type;
                           New_Col: Col_Type;
                           Move   : Boolean := True);
   pragma Inline (Set_Position);

   -- This procedure changes the position of the cursor to the one of another.
   procedure Set_Position (Cursor : in out Cursor_Type;
                           Cursor2: in     Ansi.Cursor_Type;
                           Move   : Boolean := True);
   pragma Inline (Set_Position);

   -- This procedure changes the row of the cursor.
   procedure Set_Row (Cursor : in out Cursor_Type;
                      New_Row: Row_Type;
                      Move   : Boolean := True);
   pragma Inline (Set_Row);
   
   -- This procedure changes the column of the cursor.
   procedure Set_Col (Cursor : in out Cursor_Type;
                      New_Col: Col_Type;
                      Move   : Boolean := True);
   pragma Inline (Set_Col);


   -- This procedure returns the position in rows and columns of the cursor.
   procedure Get_Position (Cursor : in  Cursor_Type;
                           Out_Row: out Row_Type;
                           Out_Col: out Col_Type);
   pragma Inline (Get_Position);

   -- This procedure returns the row where the cursor is.
   function  Get_Row (Cursor: in Cursor_Type)
                      return Row_Type;
   pragma Inline (Get_Row);

   -- This procedure returns the column where the cursor. is.
   function  Get_Col (Cursor: in Cursor_Type)
                      return Col_Type;
   pragma Inline (Get_Col);


   -- This procedure moves the cursor up (by default one).
   procedure Move_Up    (Cursor: in out Cursor_Type;
                         Rows  : Positive := 1;
                         Move  : Boolean  := True);
   pragma Inline (Move_Up);

   -- This procedure moves the cursor down (by default one).
   procedure Move_Down  (Cursor: in out Cursor_Type;
                         Rows  : Positive := 1;
                         Move  : Boolean  := True);
   pragma Inline (Move_Down);
   
   -- This procedure moves the cursor to the right (by default one).
   procedure Move_Right (Cursor: in out Cursor_Type;
                         Cols  : Positive := 1;
                         Move  : Boolean  := True);
   pragma Inline (Move_Right);
   
   -- This procedure moves the cursor to the left (by default one).
   procedure Move_Left  (Cursor: in out Cursor_Type;
                         Cols  : Positive := 1;
                         Move  : Boolean  := True);
   pragma Inline (Move_Left);

private

   type Cursor_Type is tagged limited
      record
         Row: Row_Type;
         Col: Col_Type;
      end record;

   
end Ansi.Cursors;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
