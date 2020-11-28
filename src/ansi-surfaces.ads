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

--
-- @summary
-- This package contains functions to work with surfaces and layerer.
--
-- @description
-- This package contains functions to work with surfaces (declared at Ansi) and
-- a new type called Layerer which is an optimised set of surfaces called
-- layers that can be moved and printed in a faster way than individual
-- surfaces.
--
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
   -- This part contains pocedures and functions (which aren't primitive) for
   -- the surfaces. The Surface.<Method> notation here is forbidden.

   --
   -- This function returns a copy of a surface. The copied surface isn't
   -- protected.
   --
   -- @param Surface
   -- The surface we want to copy.
   --
   -- @return
   -- It returns a complete copy of the surface.
   --
   function Copy (Surface: Surface_Type)
                  return Surface_Type;

   --
   -- This function is a shortcut to create surfaces without requiring to use
   -- the Surface_Record type new notation.
   --
   -- @param Height
   -- The height of the new surface.
   --
   -- @param Width
   -- The width of the new surface.
   --
   -- @return
   -- It returns the created empty surface.
   --
   function Create (Height: Row_Type;
                    Width : Col_Type)
                    return Surface_Type;

   --
   -- This function creates a new surface using an array of strings as the
   -- default surface.
   --
   -- @param Grid
   -- This is a 2D array of Char_Type(s) that will be converted into a surface.
   --
   -- @return
   -- It returns the created surface.
   function Create (Grid: Str_Type_Array)
                    return Surface_Type;

   --
   -- This procedure frees a surface and everything inside it.
   --
   -- @param Surface
   -- The surface to free.
   --
   procedure Free (Surface: in out Surface_Type);

   --
   -- This function returns the character in a certain position.
   --
   -- @param Surface
   -- The surface from where to take the character.
   --
   -- @param Row
   -- The row where the character is located.
   --
   -- @param Col
   -- The column where the character is located.
   --
   -- @return
   -- It returns the character in the given position.
   --
   -- @exception Ansi.Exceptions.Out_Of_Bounds_Issue.
   -- It's raised if either the row, the column or both are out of the
   -- Surface's bounds.
   --
   function Get_Char (Surface: Surface_Type;
                      Row    : Row_Type;
                      Col    : Col_Type)
                      return Char_Type;

   --
   -- This function returns the cursor of the surface so it can be moved.
   --
   -- @param Surface
   -- The surface containing the cursor.
   --
   -- @return
   -- It returns the surface's cursor.
   --
   function Get_Cursor (Surface: Surface_Type)
                        return Cursor_Type;

   --
   -- This procedure pastes a surface onto another. The position of the surface
   -- mustn't be out of bounds. But the the surface pasted can be bigger than
   -- the one it's pasted over.
   --
   -- @param Over
   -- The surface to paste it over.
   --
   -- @param Surface
   -- The surface to paste over it.
   --
   -- @param Row
   -- The row in which the new surface will be pasted over.
   --
   -- @param Col
   -- The column where the new surface will be pasted over.
   --
   -- @exception Ansi.Exceptions.Out_Of_Bounds_Issue
   -- This exceptions is raised if the position where the surface it's pasted
   -- it's out of range.
   --
   procedure Paste (Over   : Surface_Type;
                    Surface: Surface_Type;
                    Row    : Row_Type;
                    Col    : Col_Type)
                    with Pre => Row <= Over.Height and
                                Col <= Over.Width;
   
   --
   -- This procedure prints a string into a surface. If the surface is null, it
   -- points to the main surface. It raises an error if the string goes out of
   -- ranges but it writes it until it can. It also moves the cursor one space
   -- to the left of the place where it ended. (Out_Of_Bounds_Issue)
   --
   -- @param Item
   -- The string to put onto the surface.
   --
   -- @param Surface
   -- The surface to be written. If it is null, then it's written to the
   -- Main_Surface.
   --
   -- @param Feed
   -- If this parameter is set to true, an error isn't raised when it goes out
   -- of bounds, but it continues into the next line.
   --
   -- @exception Ansi.Exceptions.Out_Of_Bounds_Issue
   -- This exception is raised if trying to print out of bounds. However, the
   -- procedure writes at much as possible.
   --
   procedure Put (Item   : Str_Type;
                  Surface: Surface_Type := null;
                  Feed   : Boolean      := False);

   --
   -- This procedure prints a character into a surface. If the surface is null,
   -- it points to the main surface. It raises an error if the character goes
   -- out of bounds. (Out_Of_Bounds_Issue)
   --
   -- @param Item
   -- The character to put onto the surface.
   --
   -- @param Surface
   -- The surface to be written. If it is null, then it's written to the
   -- Main_Surface.
   --
   -- @param Feed
   -- If this parameter is set to True, an error isn't raised when it goes out
   -- of bounds, but it continues into the next line.
   --
   -- @exception Ansi.Exceptions.Out_Of_Bounds_Issue
   -- This exception is raised when trying to print out of bounds.
   --
   procedure Put (Item   : Char_Type;
                  Surface: Surface_Type := null;
                  Feed   : Boolean      := False);

   --
   -- This procedure prints the fastest way possible using all the availble
   -- resources the surface onto the screen. It also frees the queue with all
   -- the changes done into the surface.
   --
   -- @param Surface
   -- The surface to be printed, if it's a null surface, then Main_Surface it's
   -- printed.
   --
   -- @param Row
   -- The row on the screen to put it into.
   --
   -- @param Col
   -- The column on the screen to put it into.
   --
   procedure Put (Surface: Surface_Type := null;
                  Row    : Row_Type     := 1;
                  Col    : Col_Type     := 1);

   --
   -- This procedure reads a string in a certain position.
   --
   -- @param Surface
   -- The surface to read.
   --
   -- @param Row
   -- The row to start reading
   --
   -- @param Col
   -- The column to start reading
   --
   -- @param Length
   -- The lenght of the string to be read.
   --
   -- @return
   -- It returns the string of a given length found in the given position of a
   -- given surface.
   --
   -- @exception Ansi.Exceptions.Out_Of_Bounds_Issue.
   -- It's raised when trying to read from out of bounds or the string's length
   -- makes the function read out of bounds.
   --
   function Read (Surface: Surface_Type;
                  Row    : Row_Type;
                  Col    : Col_Type;
                  Length : Positive)
                  return Str_Type;

   --
   -- This procedure resizes a surface adding some rows up and down and columns
   -- left and right. If the number is negative, the size is reduced. The
   -- minimum size is 1x1.
   --
   -- @param Surface
   -- The surface to resize.
   --
   -- @param Rows_Up
   -- The number of rows to add/remove  on/from the  top of the surface.
   --
   -- @param Rows_Down
   -- The number of rows to add/remove bellow the surface.
   --
   -- @param Cols_Left
   -- The number of columns to add/remove to/from the left.
   --
   -- @param Cols_Right
   -- The number of columns to add/remove to/from the right.
   --
   -- @exception Ansi.Exceptions.Windows_Size_Issue
   -- This exception is raised if the size of the surface is negative either in
   -- the number of column or the number of rows.
   --
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
   
   --
   -- This procedure adds a new layer into a layerer. If the position is 0 then
   -- it's placed at the end.
   --
   -- @param Layerer
   -- The layerer in which the new layer will be added.
   --
   -- @param Layer
   -- The layer to add to the layerer.
   --
   -- @param Position
   -- The position of the layer, if it's 0, then the layer is placed at the end
   -- of the layerer.
   --
   -- @exception Ansi.Exceptions.Already_Inside_Layerer_Issue
   -- This exception is raised if the layer is already inside the layerer.
   --
   -- @exception Ansi.Exceptions.Out_Of_Bounds_Issue
   -- This exception is raised if the layer is trying to be placed outside the
   -- ranges of the Layerer.
   --
   procedure Add (Layerer : in out Layerer_Type;
                  Layer   : Surface_Type;
                  Position: Natural := 0);

   --
   -- This procedure removes a layer from the layerer.
   --
   -- @param Layerer
   -- The layerer that contains the layer we want to remove.
   --
   -- @param Layer
   -- The layer we want to remove from the layerer.
   --
   -- @exception Ansi.Exceptions.Unknown_Layer_Issue
   -- This exception is raised if the layer we are trying to remove isn't in
   -- the layerer.
   --
   procedure Remove (Layerer: in out Layerer_Type;
                     Layer  : Surface_Type);


   --
   -- This procedure removes a layer from a certain position. The surface isn't
   -- freed though.
   --
   -- @param Layerer
   -- The layerer that contains the layer we are trying to remove.
   --
   -- @param Position
   -- The position of the layer we are trying to remove.
   --
   -- @exception Ansi.Exceptions.Out_Of_Bounds_Issue
   -- This exception is raised if the position is out of the layerer's bounds.
   --
   procedure Remove (Layerer : in out Layerer_Type;
                     Position: Positive);


   --
   -- This function checks whether there is already a layer in layerer.
   --
   -- @param Layerer
   -- The layerer.
   --
   -- @param Layer
   -- The layer we search inside the layerer.
   --
   -- @return
   -- Whether there is such layer in the layerer.
   --
   function Is_In (Layerer: Layerer_Type;
                   Layer  : Surface_Type)
                   return Boolean;
   
   --
   -- This function renames Is_In to make it simpler to use.
   --
   -- @param Left
   -- The layerer.
   --
   -- @param Right
   -- The surface type.
   --
   -- @return
   -- Is_In
   function "/" (Left : Layerer_Type;
                 Right: Surface_Type)
                 return Boolean
                 renames Is_In;

   
   --
   -- This procedure updates the layers with the minimum number of changes
   -- possible and prints them into the screen.
   -- TODO: Use paste function to avoid code repetition.
   --
   -- @param Layerer
   -- The layerer we want to update.
   --
   procedure Update (Layerer: Layerer_Type);


   --
   -- This procedure hides a layer inside the layerer.
   --
   -- @param Layerer
   -- The layerer in which the layer we want to hide is found.
   --
   -- @param Layer
   -- The layer we want to hide.
   --
   -- @exception Ansi.Exceptions.Unknown_Layer_Issue
   -- This exception is raised if the layer we are trying to hide isn't in the
   -- layerer.
   procedure Hide (Layerer: in out Layerer_Type;
                   Layer  : Surface_Type);

   --
   -- This procedure hides a layer inside all the layers.
   --
   -- @param Layerer
   -- The layerer where the layer we want to hide is found.
   --
   -- @param Position
   -- The position of such layer (the one we want to hide).
   --
   -- @exception Ansi.Exceptions.Out_Of_Bounds_Issue
   -- This exception is raised if the position is out of bounds.
   --
   procedure Hide (Layerer : in out Layerer_Type;
                   Position: Positive);

   --
   -- This procedure shows a layer inside all the layers.
   --
   -- @param Layerer
   -- The layerer which contains the layer we want to show.
   --
   -- @param Layer
   -- The layer we want to show.
   --
   -- @exception Ansi.Exceptions.Unknown_Layer_Issue
   -- This exception is raised if the layerer doesn't contain the layer we want
   -- to show.
   --
   procedure Show (Layerer: in out Layerer_Type;
                   Layer  : Surface_Type);

   --
   -- This procedure shows a layer inside all the layers.
   --
   -- @param Layerer
   -- The layerer which contains the layer we want to show.
   --
   -- @param Position
   -- The position the layer we want to show is located in the layerer.
   --
   -- @exception Ansi.Exceptions.Out_Of_Bounds_Issue
   -- This exceptions is raised if the layerer is out of the layerer's range.
   --
   procedure Show (Layerer : in out Layerer_Type;
                   Position: Positive);



   --
   -- This function returns the number of layers.
   --
   -- @param Layerer
   -- The layerer from which the number of layers it contains will be
   -- retrieved and returned.
   --
   -- @return
   -- It returns the number of layers it contains.
   --
   function Get_Layer_Number (Layerer: in Layerer_Type)
                              return Natural;
   pragma Inline (Get_Layer_Number);

   --
   -- This function returns the position of a layer. The layer on top is the
   -- first one.
   --
   -- @param Layerer
   -- The layerer in which the layer is located.
   --
   -- @param Layer
   -- The layer we are searching.
   --
   -- @return
   -- The position of such layer inside the layerer.
   --
   -- @exception Ansi.Exceptions.Unkown_Layer_Issue
   -- This exceptions is raised if the layer wasn't found in the layerer.
   --
   function Get_Position (Layerer: in Layerer_Type;
                          Layer  : not null Surface_Type)
                          return Positive;

   --
   -- This function returns a layer in certain position.
   --
   -- @param Layerer
   -- The layerer that contains the layer we are trying to get.
   --
   -- @param Position
   -- The position of that same layer.
   --
   -- @return
   -- The layer in such position.
   --
   -- @exception Ansi.Exceptions.Out_Of_Bounds_Issue
   -- This exception is raised if the position is out of bounds.
   --
   function Get_Layer (Layerer : in Layerer_Type;
                       Position: Positive)
                       return not null Surface_Type;
   pragma Inline (Get_Layer);

   --
   -- This function returns whether a layer is visible.
   --
   -- @param Layerer
   -- The layerer that contains the layer.
   --
   -- @param Layer
   -- The layer we are retrieving the visibility from.
   --
   -- @return
   -- Whether the given layer is visible or not inside the given layerer.
   --
   function Get_Visibility (Layerer: in Layerer_Type;
                            Layer  : Surface_Type)
                            return Boolean;

   --
   -- This function returns whether a layer is visible.
   --
   -- @param Layerer
   -- The layerer that contains the layer.
   --
   -- @param Position
   -- The position of the layer.
   --
   -- @return
   -- Whether the given layer is visible or not inside the given layerer.
   --
   function Get_Visibility (Layerer : in Layerer_Type;
                            Position: Positive)
                            return Boolean;
   pragma Inline (Get_Visibility);

   --
   -- This procedure moves a layer from a position to another inside the
   -- layerer.
   --
   -- @param Layerer
   -- The layerer where we want to move a layer.
   --
   -- @param Layer
   -- The layer we want to move.
   --
   -- @param To
   -- The position we want to move the layer To.
   --
   -- @exception Ansi.Exceptions.Unknown_Layer_Issue
   -- This exception is raised if the given layer isn't in the layerer.
   --
   -- @exception Ansi.Exceptions.Out_Of_Bounds_Issue
   -- This exception is raised if the destination position of the layer is out
   -- of the layerer's bounds.
   --
   procedure Move_Layer (Layerer: in Layerer_Type;
                         Layer  : Surface_Type;
                         To     : Positive);
   --
   -- This procedure moves a layer from a position to another inside the
   -- layerer.
   --
   -- @param Layerer
   -- The layerer where we want to move a layer.
   --
   -- @param From
   -- The position From we want to move the layer.
   --
   -- @param To
   -- The position we want to move the layer To.
   --
   -- @exception Ansi.Exceptions.Out_Of_Bounds_Issue
   -- This exception is raised if either the From, the To or both positions are
   -- out of the layerer's bounds.
   --
   procedure Move_Layer (Layerer: in Layerer_Type;
                         From   : Positive;
                         To     : Positive);


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
         -- The array of layers, which by default has only one item.
         Layers : Layer_Array      := new Surface_Array(1 .. 0);

         -- The visibility of each of the layers inside the layers array.
         Visible: Visibility_Array := new Boolean_Array(1 .. 0);

         -- The size of the layerer (number of layers)
         Size   : Natural := 0;
      end record;

   The_Layers: Layerer_Type;

   Lock: Boolean := False;

end Ansi.Surfaces;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
