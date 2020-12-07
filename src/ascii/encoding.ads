-------------------------------------------------------------------------------
--                                                                           --
--                          E N C O D I N G . A D S                          --
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

with Toolbox;
use Toolbox;

--
-- @summary
--
--
-- @description
--
package Encoding is


   -- TODO: Add other functions to convert it the other way.

   --
   -- This function converts a Str_Type into a normal String.
   --
   -- @param Source
   -- The string to convert.
   --
   function Convert (Source: Str_Type)
                     return String;
   pragma Pure_Function (Convert);

   --
   -- This function converts a Char_Type into a normal String.
   --
   -- @param Source
   -- The character to convert.
   --
   function Convert (Source: Char_Type)
                     return String;
   pragma Pure_Function (Convert);



   -- UC_C_Cedilla 'Ç'
   UC_C_Cedilla                          : constant Char_Type
                                         := Char_Type'Val(16#0080#);

   -- LC_Dieresis 'ü'
   LC_Dieresis                           : constant Char_Type
                                         := Char_Type'Val(16#0081#);

   -- LC_E_Acute 'é'
   LC_E_Acute                            : constant Char_Type
                                         := Char_Type'Val(16#0082#);

   -- LC_A_Circumflex 'â'
   LC_A_Circumflex                       : constant Char_Type
                                         := Char_Type'Val(16#0083#);

   -- LC_A_Dieresis 'ä'
   LC_A_Dieresis                         : constant Char_Type
                                         := Char_Type'Val(16#0084#);

   -- LC_A_Grave 'à'
   LC_A_Grave                            : constant Char_Type
                                         := Char_Type'Val(16#0085#);

   -- LC_A_Ring 'å'
   LC_A_Ring                             : constant Char_Type
                                         := Char_Type'Val(16#0086#);

   -- LC_C_Cedilla 'ç'
   LC_C_Cedilla                          : constant Char_Type
                                         := Char_Type'Val(16#0087#);

   -- LC_E_Circumflex 'ê'
   LC_E_Circumflex                       : constant Char_Type
                                         := Char_Type'Val(16#0088#);

   -- LC_E_Dieresis 'ë'
   LC_E_Dieresis                         : constant Char_Type
                                         := Char_Type'Val(16#0089#);

   -- LC_E_Grage 'è'
   LC_E_Grage                            : constant Char_Type
                                         := Char_Type'Val(16#008A#);

   -- LC_I_Dieresis 'ï'
   LC_I_Dieresis                         : constant Char_Type
                                         := Char_Type'Val(16#008B#);

   -- LC_I_Circumflex 'î'
   LC_I_Circumflex                       : constant Char_Type
                                         := Char_Type'Val(16#008C#);

   -- LC_I_Grave 'ì'
   LC_I_Grave                            : constant Char_Type
                                         := Char_Type'Val(16#008D#);

   -- UC_A_Dieresis 'Ä'
   UC_A_Dieresis                         : constant Char_Type
                                         := Char_Type'Val(16#008E#);

   -- UC_A_Ring 'Å'
   UC_A_Ring                             : constant Char_Type
                                         := Char_Type'Val(16#008F#);

   -- UC_E_Acute 'É'
   UC_E_Acute                            : constant Char_Type
                                         := Char_Type'Val(16#0090#);

   -- LC_AE_Diphthong 'æ'
   LC_AE_Diphthong                       : constant Char_Type
                                         := Char_Type'Val(16#0091#);

   -- UC_AE_Diphthong 'Æ'
   UC_AE_Diphthong                       : constant Char_Type
                                         := Char_Type'Val(16#0092#);

   -- LC_O_Circumflex 'ô'
   LC_O_Circumflex                       : constant Char_Type
                                         := Char_Type'Val(16#0093#);

   -- LC_O_Dieresis 'ö'
   LC_O_Dieresis                         : constant Char_Type
                                         := Char_Type'Val(16#0094#);

   -- LC_O_Grave 'ò'
   LC_O_Grave                            : constant Char_Type
                                         := Char_Type'Val(16#0095#);

   -- LC_U_Circumflex 'û'
   LC_U_Circumflex                       : constant Char_Type
                                         := Char_Type'Val(16#0096#);

   -- LC_U_Grave 'ù'
   LC_U_Grave                            : constant Char_Type
                                         := Char_Type'Val(16#0097#);

   -- LC_Y_Dieresis 'ÿ'
   LC_Y_Dieresis                         : constant Char_Type
                                         := Char_Type'Val(16#0098#);

   -- UC_O_Dieresis 'Ö'
   UC_O_Dieresis                         : constant Char_Type
                                         := Char_Type'Val(16#0099#);

   -- UC_U_Dieresis 'Ü'
   UC_U_Dieresis                         : constant Char_Type
                                         := Char_Type'Val(16#009A#);

   -- Cent_Sign '¢'
   Cent_Sign                             : constant Char_Type
                                         := Char_Type'Val(16#009B#);

   -- Pound_Sign '£'
   Pound_Sign                            : constant Char_Type
                                         := Char_Type'Val(16#009C#);

   -- Yen_Sign '¥'
   Yen_Sign                              : constant Char_Type
                                         := Char_Type'Val(16#009D#);

   -- Something_1 '₧'
   Something_1                           : constant Char_Type
                                         := Char_Type'Val(16#009E#);

   -- Forte 'ƒ'
   Forte                                 : constant Char_Type
                                         := Char_Type'Val(16#009F#);

   -- LC_A_Acute 'á'
   LC_A_Acute                            : constant Char_Type
                                         := Char_Type'Val(16#00A0#);

   -- LC_I_Acute 'í'
   LC_I_Acute                            : constant Char_Type
                                         := Char_Type'Val(16#00A1#);

   -- LC_O_Acute 'ó'
   LC_O_Acute                            : constant Char_Type
                                         := Char_Type'Val(16#00A2#);

   -- LC_U_Acute 'ú'
   LC_U_Acute                            : constant Char_Type
                                         := Char_Type'Val(16#00A3#);

   -- LC_N_Tilde 'ñ'
   LC_N_Tilde                            : constant Char_Type
                                         := Char_Type'Val(16#00A4#);

   -- UC_N_Tilde 'Ñ'
   UC_N_Tilde                            : constant Char_Type
                                         := Char_Type'Val(16#00A5#);

   -- Feminine_Ordinal_Indicator 'ª'
   Feminine_Ordinal_Indicator            : constant Char_Type
                                         := Char_Type'Val(16#00A6#);

   -- Masculine_Ordinal_Indicator 'º'
   Masculine_Ordinal_Indicator           : constant Char_Type
                                         := Char_Type'Val(16#00A7#);

   -- Inverted_Question '¿'
   Inverted_Question                     : constant Char_Type
                                         := Char_Type'Val(16#00A8#);

   -- Inverted_Negation '⌐'
   Inverted_Negation                     : constant Char_Type
                                         := Char_Type'Val(16#00A9#);

   -- Negation '¬'
   Negation                              : constant Char_Type
                                         := Char_Type'Val(16#00AA#);

   -- One_Half '½'
   One_Half                              : constant Char_Type
                                         := Char_Type'Val(16#00AB#);

   -- One_Forth '¼'
   One_Forth                             : constant Char_Type
                                         := Char_Type'Val(16#00AC#);

   -- Inverted_Exclamation '¡'
   Inverted_Exclamation                  : constant Char_Type
                                         := Char_Type'Val(16#00AD#);

   -- Left_Angle_Quotation '«'
   Left_Angle_Quotation                  : constant Char_Type
                                         := Char_Type'Val(16#00AE#);

   -- Right_Angle_Quotation '»'
   Right_Angle_Quotation                 : constant Char_Type
                                         := Char_Type'Val(16#00AF#);

   -- D_Block_Dark '░'
   D_Block_Dark                          : constant Char_Type
                                         := Char_Type'Val(16#00B0#);

   -- D_Block_Normal '▒'
   D_Block_Normal                        : constant Char_Type
                                         := Char_Type'Val(16#00B1#);

   -- D_Block_Dim '▓'
   D_Block_Dim                           : constant Char_Type
                                         := Char_Type'Val(16#00B2#);

   -- D_Vertical '│'
   D_Vertical                            : constant Char_Type
                                         := Char_Type'Val(16#00B3#);

   -- D_Vertical_Middle_Left '┤'
   D_Vertical_Middle_Left                : constant Char_Type
                                         := Char_Type'Val(16#00B4#);

   -- D_Vertical_Double_Middle_Left '╡'
   D_Vertical_Double_Middle_Left         : constant Char_Type
                                         := Char_Type'Val(16#00B5#);

   -- D_Double_Vertical_Middle_Left '╢'
   D_Double_Vertical_Middle_Left         : constant Char_Type
                                         := Char_Type'Val(16#00B6#);

   -- D_Top_Right_Corner_Double_Down '╖'
   D_Top_Right_Corner_Double_Down        : constant Char_Type
                                         := Char_Type'Val(16#00B7#);

   -- D_Top_Right_Corner_Double_Top '╕'
   D_Top_Right_Corner_Double_Top         : constant Char_Type
                                         := Char_Type'Val(16#00B8#);

   -- D_Double_Vertical_Double_Middle_Left '╣'
   D_Double_Vertical_Double_Middle_Left  : constant Char_Type
                                         := Char_Type'Val(16#00B9#);

   -- D_Double_Vertical '║'
   D_Double_Vertical                     : constant Char_Type
                                         := Char_Type'Val(16#00BA#);

   -- D_Double_Top_Right_Corner '╗'
   D_Double_Top_Right_Corner             : constant Char_Type
                                         := Char_Type'Val(16#00BB#);

   -- D_Double_Bottom_Right_Corner '╝'
   D_Double_Bottom_Right_Corner          : constant Char_Type
                                         := Char_Type'Val(16#00BC#);

   -- D_Bottom_Right_Corner_Double_Right '╜'
   D_Bottom_Right_Corner_Double_Right    : constant Char_Type
                                         := Char_Type'Val(16#00BD#);

   -- D_Bottom_Right_Corner_Double_Bottom '╛'
   D_Bottom_Right_Corner_Double_Bottom   : constant Char_Type
                                         := Char_Type'Val(16#00BE#);

   -- D_Top_Right_Corner '┐'
   D_Top_Right_Corner                    : constant Char_Type
                                         := Char_Type'Val(16#00BF#);

   -- D_Bottom_Left_Corner '└'
   D_Bottom_Left_Corner                  : constant Char_Type
                                         := Char_Type'Val(16#00C0#);

   -- D_Horizontal_Middle_Up '┴'
   D_Horizontal_Middle_Up                : constant Char_Type
                                         := Char_Type'Val(16#00C1#);

   -- D_Horizontal_Middle_Down '┬'
   D_Horizontal_Middle_Down              : constant Char_Type
                                         := Char_Type'Val(16#00C2#);

   -- D_Vertical_Middle_Right '├'
   D_Vertical_Middle_Right               : constant Char_Type
                                         := Char_Type'Val(16#00C3#);

   -- D_Horizontal '─'
   D_Horizontal                          : constant Char_Type
                                         := Char_Type'Val(16#00C4#);

   -- D_Four_Directions '┼'
   D_Four_Directions                     : constant Char_Type
                                         := Char_Type'Val(16#00C5#);

   -- D_Vertical_Double_Middle_Right '╞'
   D_Vertical_Double_Middle_Right        : constant Char_Type
                                         := Char_Type'Val(16#00C6#);

   -- D_Double_Vertical_Middle_Right '╟'
   D_Double_Vertical_Middle_Right        : constant Char_Type
                                         := Char_Type'Val(16#00C7#);

   -- D_Double_Bottom_Left_Corner '╚'
   D_Double_Bottom_Left_Corner           : constant Char_Type
                                         := Char_Type'Val(16#00C8#);

   -- D_Double_Top_Left_Corner '╔'
   D_Double_Top_Left_Corner              : constant Char_Type
                                         := Char_Type'Val(16#00C9#);

   -- D_Double_Horizontal_Double_Middle_Up '╩'
   D_Double_Horizontal_Double_Middle_Up  : constant Char_Type
                                         := Char_Type'Val(16#00CA#);

   -- D_Double_Horizontal_Double_Middle_Down '╦'
   D_Double_Horizontal_Double_Middle_Down: constant Char_Type
                                         := Char_Type'Val(16#00CB#);

   -- D_Double_Vertical_Double_Middle_Right '╠'
   D_Double_Vertical_Double_Middle_Right : constant Char_Type
                                         := Char_Type'Val(16#00CC#);

   -- D_Double_Horizontal '═'
   D_Double_Horizontal                   : constant Char_Type
                                         := Char_Type'Val(16#00CD#);

   -- D_Double_Four_Directions '╬'
   D_Double_Four_Directions              : constant Char_Type
                                         := Char_Type'Val(16#00CE#);

   -- D_Double_Horizontal_Middle_Up '╧'
   D_Double_Horizontal_Middle_Up         : constant Char_Type
                                         := Char_Type'Val(16#00CF#);

   -- D_Horizontal_Double_Middle_Up '╨'
   D_Horizontal_Double_Middle_Up         : constant Char_Type
                                         := Char_Type'Val(16#00D0#);

   -- D_Double_Horizontal_Middle_Down '╤'
   D_Double_Horizontal_Middle_Down       : constant Char_Type
                                         := Char_Type'Val(16#00D1#);

   -- D_Horizontal_Double_Middle_Down '╥'
   D_Horizontal_Double_Middle_Down       : constant Char_Type
                                         := Char_Type'Val(16#00D2#);

   -- D_Bottom_Left_Corner_Double_Left '╙'
   D_Bottom_Left_Corner_Double_Left      : constant Char_Type
                                         := Char_Type'Val(16#00D3#);

   -- D_Bottom_Left_Corner_Double_Bottom '╘'
   D_Bottom_Left_Corner_Double_Bottom    : constant Char_Type
                                         := Char_Type'Val(16#00D4#);

   -- D_Top_Left_Corner_Double_Top '╒'
   D_Top_Left_Corner_Double_Top          : constant Char_Type
                                         := Char_Type'Val(16#00D5#);

   -- D_Top_Left_Corner_Double_Left '╓'
   D_Top_Left_Corner_Double_Left         : constant Char_Type
                                         := Char_Type'Val(16#00D6#);

   -- D_Double_Vertical_Middle_Horizontal '╫'
   D_Double_Vertical_Middle_Horizontal   : constant Char_Type
                                         := Char_Type'Val(16#00D7#);

   -- D_Double_Horizontal_Middle_Vertical '╪'
   D_Double_Horizontal_Middle_Vertical   : constant Char_Type
                                         := Char_Type'Val(16#00D8#);

   -- D_Bottom_Right_Corner '┘'
   D_Bottom_Right_Corner                 : constant Char_Type
                                         := Char_Type'Val(16#00D9#);

   -- D_Top_Left_Corner '┌'
   D_Top_Left_Corner                     : constant Char_Type
                                         := Char_Type'Val(16#00DA#);

   -- D_Block '█'
   D_Block                               : constant Char_Type
                                         := Char_Type'Val(16#00DB#);

   -- D_Half_Down_Block '▄'
   D_Half_Down_Block                     : constant Char_Type
                                         := Char_Type'Val(16#00DC#);

   -- D_Half_Left_Block '▌'
   D_Half_Left_Block                     : constant Char_Type
                                         := Char_Type'Val(16#00DD#);

   -- D_Half_Right_Block '▐'
   D_Half_Right_Block                    : constant Char_Type
                                         := Char_Type'Val(16#00DE#);

   -- D_Half_Up_Block '▀'
   D_Half_Up_Block                       : constant Char_Type
                                         := Char_Type'Val(16#00DF#);

   -- LC_Alpha 'α'
   LC_Alpha                              : constant Char_Type
                                         := Char_Type'Val(16#00E0#);

   -- LC_Beta 'ß'
   LC_Beta                               : constant Char_Type
                                         := Char_Type'Val(16#00E1#);

   -- UC_Gamma 'Γ'
   UC_Gamma                              : constant Char_Type
                                         := Char_Type'Val(16#00E2#);

   -- LC_Pi 'π'
   LC_Pi                                 : constant Char_Type
                                         := Char_Type'Val(16#00E3#);

   -- UC_Sigma 'Σ'
   UC_Sigma                              : constant Char_Type
                                         := Char_Type'Val(16#00E4#);

   -- LC_Sigma 'σ'
   LC_Sigma                              : constant Char_Type
                                         := Char_Type'Val(16#00E5#);

   -- LC_Mu 'µ'
   LC_Mu                                 : constant Char_Type
                                         := Char_Type'Val(16#00E6#);

   -- LC_Tau 'τ'
   LC_Tau                                : constant Char_Type
                                         := Char_Type'Val(16#00E7#);

   -- LC_Theta 'Φ'
   LC_Theta                              : constant Char_Type
                                         := Char_Type'Val(16#00E8#);

   -- UC_Theta 'Θ'
   UC_Theta                              : constant Char_Type
                                         := Char_Type'Val(16#00E9#);

   -- UC_Omega 'Ω'
   UC_Omega                              : constant Char_Type
                                         := Char_Type'Val(16#00EA#);

   -- LC_Delta 'δ'
   LC_Delta                              : constant Char_Type
                                         := Char_Type'Val(16#00EB#);

   -- Infinity '∞'
   Infinity                              : constant Char_Type
                                         := Char_Type'Val(16#00EC#);

   -- LC_Phy 'φ'
   LC_Phy                                : constant Char_Type
                                         := Char_Type'Val(16#00ED#);

   -- LC_Epsilon 'ε'
   LC_Epsilon                            : constant Char_Type
                                         := Char_Type'Val(16#00EE#);

   -- Set_Intersection '∩'
   Set_Intersection                      : constant Char_Type
                                         := Char_Type'Val(16#00EF#);

   -- Congruent '≡'
   Congruent                             : constant Char_Type
                                         := Char_Type'Val(16#00F0#);

   -- Plus_Minus '±'
   Plus_Minus                            : constant Char_Type
                                         := Char_Type'Val(16#00F1#);

   -- Greater_Or_Equal '≥'
   Greater_Or_Equal                      : constant Char_Type
                                         := Char_Type'Val(16#00F2#);

   -- Lower_Or_Equal '≤'
   Lower_Or_Equal                        : constant Char_Type
                                         := Char_Type'Val(16#00F3#);

   -- Top_Integral '⌠'
   Top_Integral                          : constant Char_Type
                                         := Char_Type'Val(16#00F4#);

   -- Low_Integral '⌡'
   Low_Integral                          : constant Char_Type
                                         := Char_Type'Val(16#00F5#);

   -- Division '÷'
   Division                              : constant Char_Type
                                         := Char_Type'Val(16#00F6#);

   -- Approx '≈'
   Approx                                : constant Char_Type
                                         := Char_Type'Val(16#00F7#);

   -- Empty_Circle '°'
   Empty_Circle                          : constant Char_Type
                                         := Char_Type'Val(16#00F8#);

   -- Filled_Circle '∙'
   Filled_Circle                         : constant Char_Type
                                         := Char_Type'Val(16#00F9#);

   -- Dot '·'
   Dot                                   : constant Char_Type
                                         := Char_Type'Val(16#00FA#);

   -- Square_Root '√'
   Square_Root                           : constant Char_Type
                                         := Char_Type'Val(16#00FB#);

   -- Raised_N 'ⁿ'
   Raised_N                              : constant Char_Type
                                         := Char_Type'Val(16#00FC#);

   -- Raised_2 '²'
   Raised_2                              : constant Char_Type
                                         := Char_Type'Val(16#00FD#);

   -- Filled_Square '■'
   Filled_Square                         : constant Char_Type
                                         := Char_Type'Val(16#00FE#);


end Encoding;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
