with Ada.Text_IO;
with Ansi;
with Ansi.Styles;

procedure Styles is
   My_Style: Ansi.Style_Array := (Ansi.Bright     => True,
                                  Ansi.Dim        => True,
                                  Ansi.Italics    => True,
                                  Ansi.Underlined => True,
                                  Ansi.Reversed   => True);
begin

   Ansi.Styles.Put_Style(My_Style);
   Ada.Text_IO.Put_Line("Hola");

   Ansi.Finalize;
exception
   when others =>
      Ada.Text_IO.Put_Line("ERROR: An unknown error occurred");
end Styles;
