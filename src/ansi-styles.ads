-------------------------------------------------------------------------------
--                                                                           --
--                       A N S I - S T Y L E S . A D S                       --
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


-- This package contains procedures to work with styles.
package Ansi.Styles is

   pragma Elaborate_Body (Ansi.Styles);

   type Style_Type is (Bright, Dim, Italic, Underline, Reversed);
   for Style_Type use
      (Bright     => 1,
       Dim        => 2,
       Italic     => 3,
       Underline  => 4,
       Reversed   => 7);
   for Style_Type'Size use 3;

   -- It adds an style, many styles can be set at the same time.
   procedure Set_Style (Style: Style_Type);

   -- Removes a specific style. It doesn't raise any error if the style hasn't
   -- been set before.
   procedure Remove_Style (Style: Style_Type);

   -- Removes all styles without removing the colours.
   procedure Remove_All_Styles;

   -- Resets the terminal to the initial state (colours included)
   procedure Plain;

private

   Styles_Used: array (Style_Type'Range) of Boolean := (others => False);

end Ansi.Styles;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
