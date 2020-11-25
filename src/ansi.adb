-------------------------------------------------------------------------------
--                                                                           --
--                              A N S I . A D B                              --
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

with  Ada.Text_IO;
with Ansi.Cursors;
with Ansi.Exceptions;
with Ansi.Os_Utils;
with Ansi.Surfaces;

pragma Elaborate (Ansi.Cursors);
pragma Elaborate (Ansi.Os_Utils);
pragma Elaborate (Ansi.Surfaces);


package body Ansi is

   ----------------------------
   -- SURFACE_TYPE FUNCTIONS --
   ----------------------------

   procedure Set_Position (Surface: Surface_Type;
                           Row    : Row_Type;
                           Col    : Col_Type) is
   begin
      
      if Surface = Main_Surface then
         raise Ansi.Exceptions.Invalid_Surface_Issue
         with "The surface is protected!";
      elsif Surface = null then
         raise Ansi.Exceptions.Using_Null_Surface_Issue
         with "The surface is null!";
      end if;

      Surface.Row := Row;
      Surface.Col := Col;
      Surface.Update_All := True;

   end Set_Position;


   -----------------------
   -- PACKAGE FUNCTIONS --
   -----------------------

   procedure Clear is
      Str: CONSTANT String(1 .. Positive(Height) * Positive(Width)) :=
                                                      (others => ' ');
   begin

      Main_Cursor.Set_Position(1, 1, True);
      Ada.Text_IO.Put_Line(ASCII.ESC & "[0m");
      Ada.Text_IO.Put_Line(Str);
      Main_Cursor.Set_Position(1, 1, True);

   end Clear;

   
   function Get_Height return Row_Type is
   begin

      return Height;

   end Get_Height;



   function Get_Width  return Col_Type is
   begin

      return Width;

   end Get_Width;



   function Get_Main_Surface return Surface_Type is
   begin

      return Main_Surface;

   end Get_Main_Surface;



   -- TODO: Implement this once the surface package has been finished.
   procedure Update_Main_Surface is
   begin

      NULL;

   end Update_Main_Surface;


   function Update_Terminal_Size return Boolean is
   begin

      return Ansi.Os_Utils.Update_Terminal_Size;

   end Update_Terminal_Size;


   procedure Finalize is
      Tmp: Character;
   begin
      
      Ansi.Os_Utils.Clean_Up;
      Main_Cursor.Set_Position(Height - 1, Width - 1);
      Ada.Text_IO.Put_Line(ASCII.ESC & "[0m");
      Ada.Text_IO.Put("Press any key to continue...");
      Ada.Text_IO.Get_Immediate(Tmp);

   end Finalize;


-------------------------------------------------------------------------------
-- private --------------------------------------------------------------------
-------------------------------------------------------------------------------
   
   procedure Push (Surface: in out Surface_Record;
                   Cursor :        Cursor_Type) is
      Op: CONSTANT Operation := new Operation_Record;
   begin

      Op.Cursor := Cursor;
      if Surface.Tail = null then
         Surface.Tail := Op;
         Surface.Head := Op;
      else
         Surface.Tail.Next := Op;
         Surface.Tail := Op;
      end if;
     -- Op.Cursor := new Ansi.Cursors.Cursor_Type;
     -- Op.Cursor.Set_Position(Surface.Cursor, False);

   end Push;



-------------------------------------------------------------------------------
-- elaboration ----------------------------------------------------------------
-------------------------------------------------------------------------------

   Temp_Boolean: Boolean;
begin

   -- We initialize the package.
   Temp_Boolean := Update_Terminal_Size;

   -- We prepare the screen.
   Ansi.Os_Utils.Prepare;

   -- Free(Main_Surface);
   Main_Surface := Ansi.Surfaces.Create(Height, Width);
   Main_Cursor := new Cursors.Cursor_Type;

   -- Ansi.Surfaces.Put(Main_Surface, 1, 1);
   Main_Surface.Protect_It := True;
   Main_Cursor.Set_Position(1, 1);
   Main_Surface.Cursor := Main_Cursor;

end Ansi;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
