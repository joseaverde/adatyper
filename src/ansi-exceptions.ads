-------------------------------------------------------------------------------
--                                                                           --
--                   A N S I - E X C E P T I O N S . A D S                   --
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


-- @summary
-- This package contains exceptions.
--
-- @description
-- This package contains all the exceptions shared by the Ansi package tree.
--
package Ansi.Exceptions is

   -- Exceptions A.K.A issues --
   
   -- This issue is raised when trying to print something out of bounds.
   Already_Inside_Layerer_Issue  : exception;

   -- This issue is raised when the terminal or the console couldn't be
   -- initiated.
   Initialization_Issue          : exception;

   -- This issue is raised when a invalid surface is given.
   Invalid_Surface_Issue         : exception;

   -- This issue is raised when a value or a position is out of bounds.
   Out_Of_Bounds_Issue           : exception;
   
   -- This issue is raised when a unknown layer is given to the layerer.
   Unknown_Layer_Issue           : exception;

   -- This issue is raised when a null surface is used.
   Using_Null_Surface_Issue      : exception;

   -- This issue is raised when trying to resize a surface to a wrong size.
   Windows_Size_Issue            : exception;

   -- This issue is raised when trying to get a wrong type from a key.
   Wrong_Kind_Of_Key_Issue       : exception;

end Ansi.Exceptions;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
