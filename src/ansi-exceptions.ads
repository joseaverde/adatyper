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


-- This package contains exceptions.
package Ansi.Exceptions is

   -- Exceptions A.K.A issues --
   
   -- TODO: Add more comments.
   -- Raised when trying to print something out of bounds.
   Already_Inside_Layerer_Issue  : exception;
   Initialization_Issue          : exception;
   Invalid_Surface_Issue         : exception;
   Out_Of_Bounds_Issue           : exception;
   Unknown_Layer_Issue           : exception;
   Windows_Size_Issue            : exception;

end Ansi.Exceptions;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
