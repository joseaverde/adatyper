with Ada.Calendar;
with Ada.Exceptions;
with Ada.Text_IO;
with Ansi;
with Ansi.Colors;
with Ansi.Surfaces;
with Ansi.Text_IO;

procedure Speed is

   File: Ada.Text_IO.File_Type;
   use type Ada.Calendar.Time;

   Start_Time: Ada.Calendar.Time := Ada.Calendar.Clock;
   Surfaces: array (Ansi.Color_Type'Range) of Ansi.Surface_Type;
   Height: CONSTANT Ansi.Row_Type := Ansi.Get_Height;
   Width : CONSTANT Ansi.Col_Type := Ansi.Get_Width;
   Finish_Time: Ada.Calendar.Time;

begin


   for C in Surfaces'Range loop
      Surfaces(C) := Ansi.Surfaces.Create(Height, Width);
      Ansi.Colors.Set_Background(Surface  => Surfaces(C),
                                 Color    => C,
                                 Bright   => False,
                                 From_Row => 1,
                                 To_Row   => Height,
                                 From_Col => 1,
                                 To_Col   => Width);
   end loop;
   Finish_Time := Ada.Calendar.Clock;

   Ada.Text_IO.Create(Name => "tests/logs/speed.log",
                      File => File,
                      Mode => Ada.Text_IO.Out_File);
   Ada.Text_IO.Put_Line(File => File,
                        Item => "HEIGHT:" & Height'Image);
   Ada.Text_IO.Put_Line(File => File,
                        Item => "WIDTH:" & Width'Image);
   Ada.Text_IO.Put_Line(File => File,
                        Item => "Time taken to generate and set the " &
                        "background colour of" & Surfaces'Length'Image &
                        " Surfaces: " & Duration'Image(Finish_Time -
                                                Start_Time));

   Start_Time := Ada.Calendar.Clock;

   for S of Surfaces loop
      Ansi.Surfaces.Put(S, 1, 1);
   end loop;

   Finish_Time := Ada.Calendar.Clock;

   Ada.Text_IO.Put_Line(File => File,
                        Item => "Rendering time: " & 
                        Duration'Image(Finish_Time - Start_Time));

   Ansi.Finalize;
exception
   when Error: others =>
      Ansi.Finalize;
      Ada.Text_IO.Put_Line(File => Ada.Text_IO.Standard_Error,
                           Item => "Unexpected Error:");
      Ada.Text_IO.Put_Line(File => Ada.Text_IO.Standard_Error,
                           Item => Ada.Exceptions.Exception_Information(
                                          Error));
end Speed;
