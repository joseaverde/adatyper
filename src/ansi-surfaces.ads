-------------------------------------------------------------------------------
--                                                                           --
--                     A N S I - S U R F A C E S . A D S                     --
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

with Ada.Unchecked_Deallocation;


-- This package contains functions to work with surfaces.
package Ansi.Surfaces is
   
   -- The layerer type can hold many layers of Surfaces that can be then sorted
   -- and updated in that order together. This package only contains one.
   type Layerer_Type is tagged limited private;

   -- This type is used to create surfaces with data already created.
   type Str_Type_Array is array (Positive range <>,
                                 Positive range <>) of Char_Type;


   ----------------------------
   -- FUNCTIONS FOR SURFACES --
   ----------------------------
   
   -- This function returns a copy of a surface.
   function Copy (Surface: Surface_Type)
                  return Surface_Type;
   
   -- This function is a shortcut to create surfaces without requiring to use
   -- the Surface_Record type.
   function Create (Height: Row_Type;
                    Width : Col_Type)
                    return Surface_Type;
   pragma Inline (Create);

   -- This function creates a new surface using an array of strings as the
   -- default surface.
   function Create (Grid: Str_Type_Array)
                    return Surface_Type;

   -- This procedure frees a surface and everything inside it.
   procedure Free (Surface: in out Surface_Type);

   -- This function returns the cursor of the surface so it can be moved.
   function Get_Cursor (Surface: Surface_Type)
                        return Cursor_Type;
   pragma Inline (Get_Cursor);

   -- This procedure pastes a surface onto another. It doesn't raise any error
   -- if the surface it's pasted over is too small, but with the Ranges.
   procedure Paste (Over   : Surface_Type;
                    Surface: Surface_Type;
                    Row    : Row_Type;
                    Col    : Col_Type)
                    with Pre => Row <= Over.Height and
                                Col <= Over.Width;
   
   -- This procedure prints a string into a surface. If the surface is null, it
   -- points to the main surface. It raises an error if the string goes out of
   -- ranges but it writes it until it can. It also moves the cursor one space
   -- to the left of the place where it ended. (Out_Of_Bounds_Issue)
   procedure Put (Item   : Str_Type;
                  Surface: Surface_Type := null;
                  Feed   : Boolean      := False);

   -- This procedure prints a character into a surface. If the surface is null,
   -- it points to the main surface. It raises an error if the character goes
   -- out of bounds. (Out_Of_Bounds_Issue)
   procedure Put (Item   : Char_Type;
                  Surface: Surface_Type := null;
                  Feed   : Boolean      := False);

   -- This procedure forces a surface to be printed onto the screen, if it goes
   -- out of bounds it raises an exception Out_Of_Bounds_Issue. If the surface
   -- is null, then it's the main surface. It doesn't look at the tail, it's
   -- the output of the full Surface without trying to minimize the changes.
   procedure Put (Surface: Surface_Type := null;
                  Row    : Row_Type     := 1;
                  Col    : Col_Type     := 1);

   -- This procedure resizes a surface adding some rows up and down and columns
   -- left and right. If the number is negative the size is reduced. The
   -- minimum size is 1x1.
   procedure Resize (Surface   : in out not null Surface_Type;
                     Rows_Up   : Integer := 0;
                     Rows_Down : Integer := 0;
                     Cols_Left : Integer := 0;
                     Cols_Right: Integer := 0)
                     with Pre => (Integer(Surface.Height) +
                                       Rows_Up + Rows_Down) > 0 and
                                 (Integer(Surface.Width) +
                                       Cols_Left + Cols_Right) > 0;


   ---------------------
   -- LAYER FUNCTIONS --
   ---------------------
   
   -- This procedure adds a new layer into a layerer. If the position is 0 then
   -- it's placed at the end.
   procedure Add (Layerer : in out Layerer_Type;
                  Layer   : Surface_Type;
                  Position: Natural := 0);

   -- This procedure removes a layer from the layerer.
   procedure Remove (Layerer: in out Layerer_Type;
                     Layer  : Surface_Type);


   -- This procedure removes a layer from a certain position. The surface isn0t
   -- freed though.
   procedure Remove (Layerer : in out Layerer_Type;
                     Position: Positive);



   -- This function checks whether there is already a layer in layerer.
   function Is_In (Layerer: Layerer_Type;
                   Layer  : Surface_Type)
                   return Boolean;
   pragma Inline (Is_In);
   
   -- This function renames Is_In to make it simpler to use.
   function "/" (Left : Layerer_Type;
                 Right: Surface_Type)
                 return Boolean
                 renames Is_In;

   
   -- This procedure updates the layers with the minimum number of changes
   -- possible and prints them into the screen.
   procedure Update (Layerer: Layerer_Type);


   -- This procedure hides a layer inside all the layers. If the Layer isn't
   -- found, an error is raised.
   procedure Hide (Layerer: in out Layerer_Type;
                   Layer  : Surface_Type);

   -- This procedure hides a layer inside all the layers. If the Layer isn't
   -- found, an error is raised.
   procedure Hide (Layerer : in out Layerer_Type;
                   Position: Positive);

   -- This procedure shows a layer inside all the layers. The default is a
   -- layer to be shown. If the Layer isn't found, an error is raised.
   procedure Show (Layerer: in out Layerer_Type;
                   Layer  : Surface_Type);

   -- This procedure shows a layer inside all the layers. The default is a
   -- layer to be shown. If the Layer isn't found, an error is raised.
   procedure Show (Layerer : in out Layerer_Type;
                   Position: Positive);



   -- This function returns the number of layers.
   function Get_Layer_Number (Layerer: in Layerer_Type)
                              return Natural;
   pragma Inline (Get_Layer_Number);

   -- This function returns the position of a layer. The layer on top is the
   -- first one.
   function Get_Position (Layerer: in Layerer_Type;
                          Layer  : not null Surface_Type)
                          return Positive;

   -- This function returns a layer in certain position, if it's out of bounds
   -- it raises an error.
   function Get_Layer (Layerer : in Layerer_Type;
                       Position: Positive)
                       return not null Surface_Type;
   pragma Inline (Get_Layer);

   -- This function returns whether a layer is visible.
   function Get_Visibility (Layerer: in Layerer_Type;
                            Layer  : Surface_Type)
                            return Boolean;
   pragma Inline (Get_Visibility);

   -- This function returns whether a layer is visible.
   function Get_Visibility (Layerer : in Layerer_Type;
                            Position: Positive)
                            return Boolean;
   pragma Inline (Get_Visibility);

                          -- TODO: Move Move_Above Move_Bellow

private
   
   -- This type is used to store all arrays.
   type Surface_Array is array (Positive range <>) of Surface_Type;

   -- This is an access type to Surface_Array, so it can be changed in runtime.
   type Layer_Array is access Surface_Array;

   -- This type is used to store whether the layer is hidden or not.
   type Boolean_Array is array (Positive range <>) of Boolean
      with Default_Component_Value => True;

   -- This type is just an access type to Boolean_Array with a cooler name.
   type Visibility_Array is access Boolean_Array;

   procedure Free is new Ada.Unchecked_Deallocation(Surface_Array,
                                                    Layer_Array);
   procedure Free is new Ada.Unchecked_Deallocation(Boolean_Array,
                                                    Visibility_Array);

   -- The layers are stored in an array access that can upgraded in runtime,
   -- the array contains Surface_Type_s in a given order that can be changed
   -- with functions. The Main_Surface CAN'T be used as a layer because it's
   -- the one where all the changes will be done. It's a protected layer.
   type Layerer_Type is tagged limited
      record
         -- The array, which by default has only one item.
         Layers : Layer_Array      := new Surface_Array(1 .. 0);
         Visible: Visibility_Array := new Boolean_Array(1 .. 0);
         Size   : Natural := 0;
      end record;

   The_Layers: Layerer_Type;
end Ansi.Surfaces;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
