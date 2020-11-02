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
with Ansi.Surfaces;

package body Ansi is

   
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
      Ws        : Winsize;
      New_Height: Row_Type;
      New_Width : Col_Type;
      Temp_Int  : Interfaces.C.int;
   begin
      
      Temp_Int := Ioctl(Fd      => 1,  -- File descriptor = 1 (Standard output)
                        Request => TIOCGWINSZ,
                        Struct  => Ws);

      New_Height := Row_Type(Ws.ws_row);
      New_Width  := Col_Type(Ws.ws_col);

      if New_Height /= Height or New_Width /= Width then
         Height := New_Height;
         Width  := New_Width;
         return False;
      end if;

      return True;

   end Update_Terminal_Size;



   procedure Finalize is
      Tmp: Character;
   begin
      
      System_Command("stty echo");
      System_Command("tput cnorm");

      Main_Cursor.Set_Position(Height - 1, Width - 1);
      Ada.Text_IO.Put_Line(ASCII.ESC & "[0m");
      Ada.Text_IO.Get_Immediate(Tmp);
     -- Ada.Text_IO.Put_Line("Height:"&Height'Image & " ;; " &
     --                      "Width:"&Width'Image);

   end Finalize;


-------------------------------------------------------------------------------
-- private --------------------------------------------------------------------
-------------------------------------------------------------------------------
   
   procedure Push (Surface: in out Surface_Record;
                   Cursor :        Cursor_Type) is
      Op: Operation := new Operation_Record;
   begin

      Op.Next := Surface.Head;
      Op.Cursor := new Ansi.Cursors.Cursor_Type;
      Op.Cursor.Set_Position(Surface.Cursor, False);

      if Surface.Tail = null then
         Surface.Tail := Op;
      end if;
      Surface.Head := Op;

   end Push;


   procedure System_Command (Cmd: String) is
      function C_System (Cmd: Interfaces.C.Char_Array) return Integer;
      pragma Import (C, C_System, "system");
      Ret_Val: Integer;
   begin
      
      Ret_Val := C_System(Interfaces.C.To_C(Cmd));

   end System_Command;


-------------------------------------------------------------------------------
-- elaboration ----------------------------------------------------------------
-------------------------------------------------------------------------------

   Temp_Boolean: Boolean;
begin

   -- We prepare the screen.
   System_Command("stty -echo");
   System_Command("tput civis");

   -- We initialize the package.
   Temp_Boolean := Update_Terminal_Size;
   Main_Surface := Ansi.Surfaces.Create(Height, Width);
   Main_Cursor := new Cursors.Cursor_Type;

   Ansi.Surfaces.Put(Main_Surface, 1, 1);
   Main_Cursor.Set_Position(1, 1);
   Main_Surface.Cursor := Main_Cursor;

end Ansi;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
