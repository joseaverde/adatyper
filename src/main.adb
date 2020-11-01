-------------------------------------------------------------------------------
--                                                                           --
--                              M A I N . A D B                              --
--                                                                           --
--                              A D A T Y P E R                              --
--                                                                           --
--                                  M A I N                                  --
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

with Ada.Exceptions;
with Ada.Text_IO;
with Ansi;
with Ansi.Colors;
with Ansi.Cursors;
with Ansi.Styles;
with Ansi.Surfaces;
with Credits;
with System;

-- This is the main function of the program, it returns an natural nuber with
-- the error code if any occurs.
function Main return Natural is
   procedure Debug (N: Natural) is
   begin
      Ada.Text_IO.Put_Line("DEBUG:"&N'Image);
   end Debug;

   My_Surface: Ansi.Surface_Type := Ansi.Surfaces.Create(10, 20);

begin

   Ansi.Surfaces.Get_Cursor(My_Surface).Set_Position(5, 1, False);
   Ansi.Surfaces.Put("Hola mundo", My_Surface);
   Ansi.Surfaces.Get_Cursor(My_Surface).Set_Position(7, 2, False);
   Ansi.Surfaces.Put("Hello World", My_Surface);
   Ansi.Surfaces.Put(My_Surface);
   

   -- Credits.Startup_Notice;
   -- Credits.Start_UP
   
   
   Ansi.Finalize;
   return 0;

exception
   when Error: others =>
      Ansi.Finalize;
      Ada.Text_IO.Put_Line(File => Ada.Text_IO.Standard_Error,
                           Item => "Unexpected error occurred:  ");
      Ada.Text_IO.Put_Line(File => Ada.Text_IO.Standard_Error,
                           Item => Ada.Exceptions.Exception_Information(
                                       Error));

      return 255;

end Main;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
