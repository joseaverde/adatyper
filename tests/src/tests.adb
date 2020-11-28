-------------------------------------------------------------------------------
--                                                                           --
--                             T E S T S . A D B                             --
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

with Ansi;
with Ada.IO_EXCEPTIONS;
with Ada.Task_Identification; use Ada.Task_Identification;

package body Tests is

   
   procedure Error (Err: Ada.Exceptions.Exception_Occurrence) is
   begin

      Ansi.Finalize;
      Ada.Text_IO.Put_Line(File => Ada.Text_IO.Standard_Error,
                           Item => "Unexpected error occurred:   ");
      Ada.Text_IO.Put_Line(File => Ada.Text_IO.Standard_Error,
                           Item => Ada.Exceptions.Exception_Information(Err));
      if Ada.Text_IO.Is_Open(File => File) then
         Ada.Text_IO.Close(File => File);
      end if;

   end Error;


   procedure Error (Item: String) is
   begin

      Ansi.Finalize;
      Ada.Text_IO.Put_Line(File => Ada.Text_IO.Standard_Error,
                           Item => Item);
      if Ada.Text_IO.Is_Open(File => File) then
         Ada.Text_IO.Close(File => File);
      end if;
      Abort_Task(Current_Task);

   end Error;


   procedure Finalize is
   begin

      if Ada.Text_IO.Is_Open(File => File) then
         Ada.Text_IO.Close(File => File);
      end if;

   end Finalize;


   procedure Print (Item: String) is
   begin

      Ada.Text_IO.Put_Line(File => File,
                           Item => Item);

   end Print;

begin

   Ada.Text_IO.Create(File => File,
                      Mode => Ada.Text_IO.Out_File,
                      Name => "logs/temp.log");

exception
   when ADA.IO_EXCEPTIONS.NAME_ERROR =>
      null;

end Tests;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
