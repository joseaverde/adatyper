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

with Ansi.Exceptions;

package body Ansi is

   procedure Initialize is
      Temp_Boolean: Boolean;
   begin
      
      if Is_Initialized then
         raise Ansi.Exceptions.Already_Initialized_Issue
         with  "The library has already been initialized";
      end if;

      Temp_Boolean := Update_Terminal_Size;
      Create_Main_Surface;

   end Initialize;


   
   function Get_Height return Positive is
   begin

      return Height;

   end Get_Height;



   function Get_Width  return Positive is
   begin

      return Width;

   end Get_Width;



   function Get_Main_Surface return Surface is
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
      New_Height: Positive;
      New_Width : Positive;
      Temp_Int  : Interfaces.C.int;
   begin
      
      Temp_Int := Ioctl(Fd      => 1,  -- File descriptor = 1 (Standard output)
                        Request => TIOCGWINSZ,
                        Struct  => Ws);

      New_Height := Positive(Ws.ws_col);
      New_Width  := Positive(Ws.ws_row);

      if New_Height /= Height or New_Width /= Width then
         Height := New_Height;
         Width  := New_Width;
         return False;
      end if;

      return True;

   end Update_Terminal_Size;

   
   
   -- TODO: Impliment this procedure once the Ansi.Surfaces package is finished
   procedure Create_Main_Surface is
   begin

      NULL;

   end Create_Main_Surface;

end Ansi;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
