-------------------------------------------------------------------------------
--                                                                           --
--                             T E S T S . A D S                             --
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



with Ada.Exceptions;
with Ada.Text_IO;

package Tests is
   
   -- This procedure is runned everytime an unhandled exception is caught.
   procedure Error (Err: Ada.Exceptions.Exception_Occurrence);

   procedure Error (Item: String);

   -- This procedure finalizes this package.
   procedure Finalize;

   -- This procedure is used to print into a file status information.
   procedure Print (Item: String);

private

   File: Ada.Text_IO.File_Type;

end Tests;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
