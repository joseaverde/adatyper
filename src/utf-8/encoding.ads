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
                                         := Char_Type'Val(16#c387#);

   -- LC_Dieresis 'ü'
   LC_Dieresis                           : constant Char_Type
                                         := Char_Type'Val(16#c3bc#);

   -- LC_E_Acute 'é'
   LC_E_Acute                            : constant Char_Type
                                         := Char_Type'Val(16#c3a9#);

   -- LC_A_Circumflex 'â'
   LC_A_Circumflex                       : constant Char_Type
                                         := Char_Type'Val(16#c3a2#);

   -- LC_A_Dieresis 'ä'
   LC_A_Dieresis                         : constant Char_Type
                                         := Char_Type'Val(16#c3a4#);

   -- LC_A_Grave 'à'
   LC_A_Grave                            : constant Char_Type
                                         := Char_Type'Val(16#c3a0#);

   -- LC_A_Ring 'å'
   LC_A_Ring                             : constant Char_Type
                                         := Char_Type'Val(16#c3a5#);

   -- LC_C_Cedilla 'ç'
   LC_C_Cedilla                          : constant Char_Type
                                         := Char_Type'Val(16#c3a7#);

   -- LC_E_Circumflex 'ê'
   LC_E_Circumflex                       : constant Char_Type
                                         := Char_Type'Val(16#c3aa#);

   -- LC_E_Dieresis 'ë'
   LC_E_Dieresis                         : constant Char_Type
                                         := Char_Type'Val(16#c3ab#);

   -- LC_E_Grage 'è'
   LC_E_Grage                            : constant Char_Type
                                         := Char_Type'Val(16#c3a8#);

   -- LC_I_Dieresis 'ï'
   LC_I_Dieresis                         : constant Char_Type
                                         := Char_Type'Val(16#c3af#);

   -- LC_I_Circumflex 'î'
   LC_I_Circumflex                       : constant Char_Type
                                         := Char_Type'Val(16#c3ae#);

   -- LC_I_Grave 'ì'
   LC_I_Grave                            : constant Char_Type
                                         := Char_Type'Val(16#c3ac#);

   -- UC_A_Dieresis 'Ä'
   UC_A_Dieresis                         : constant Char_Type
                                         := Char_Type'Val(16#c384#);

   -- UC_A_Ring 'Å'
   UC_A_Ring                             : constant Char_Type
                                         := Char_Type'Val(16#c385#);

   -- UC_E_Acute 'É'
   UC_E_Acute                            : constant Char_Type
                                         := Char_Type'Val(16#c389#);

   -- LC_AE_Diphthong 'æ'
   LC_AE_Diphthong                       : constant Char_Type
                                         := Char_Type'Val(16#c3a6#);

   -- UC_AE_Diphthong 'Æ'
   UC_AE_Diphthong                       : constant Char_Type
                                         := Char_Type'Val(16#c386#);

   -- LC_O_Circumflex 'ô'
   LC_O_Circumflex                       : constant Char_Type
                                         := Char_Type'Val(16#c3b4#);

   -- LC_O_Dieresis 'ö'
   LC_O_Dieresis                         : constant Char_Type
                                         := Char_Type'Val(16#c3b6#);

   -- LC_O_Grave 'ò'
   LC_O_Grave                            : constant Char_Type
                                         := Char_Type'Val(16#c3b2#);

   -- LC_U_Circumflex 'û'
   LC_U_Circumflex                       : constant Char_Type
                                         := Char_Type'Val(16#c3bb#);

   -- LC_U_Grave 'ù'
   LC_U_Grave                            : constant Char_Type
                                         := Char_Type'Val(16#c3b9#);

   -- LC_Y_Dieresis 'ÿ'
   LC_Y_Dieresis                         : constant Char_Type
                                         := Char_Type'Val(16#c3bf#);

   -- UC_O_Dieresis 'Ö'
   UC_O_Dieresis                         : constant Char_Type
                                         := Char_Type'Val(16#c396#);

   -- UC_U_Dieresis 'Ü'
   UC_U_Dieresis                         : constant Char_Type
                                         := Char_Type'Val(16#c39c#);

   -- Cent_Sign '¢'
   Cent_Sign                             : constant Char_Type
                                         := Char_Type'Val(16#c2a2#);

   -- Pound_Sign '£'
   Pound_Sign                            : constant Char_Type
                                         := Char_Type'Val(16#c2a3#);

   -- Yen_Sign '¥'
   Yen_Sign                              : constant Char_Type
                                         := Char_Type'Val(16#c2a5#);

   -- Something_1 '₧'
   Something_1                           : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#82a7#));

   -- Forte 'ƒ'
   Forte                                 : constant Char_Type
                                         := Char_Type'Val(16#c692#);

   -- LC_A_Acute 'á'
   LC_A_Acute                            : constant Char_Type
                                         := Char_Type'Val(16#c3a1#);

   -- LC_I_Acute 'í'
   LC_I_Acute                            : constant Char_Type
                                         := Char_Type'Val(16#c3ad#);

   -- LC_O_Acute 'ó'
   LC_O_Acute                            : constant Char_Type
                                         := Char_Type'Val(16#c3b3#);

   -- LC_U_Acute 'ú'
   LC_U_Acute                            : constant Char_Type
                                         := Char_Type'Val(16#c3ba#);

   -- LC_N_Tilde 'ñ'
   LC_N_Tilde                            : constant Char_Type
                                         := Char_Type'Val(16#c3b1#);

   -- UC_N_Tilde 'Ñ'
   UC_N_Tilde                            : constant Char_Type
                                         := Char_Type'Val(16#c391#);

   -- Feminine_Ordinal_Indicator 'ª'
   Feminine_Ordinal_Indicator            : constant Char_Type
                                         := Char_Type'Val(16#c2aa#);

   -- Masculine_Ordinal_Indicator 'º'
   Masculine_Ordinal_Indicator           : constant Char_Type
                                         := Char_Type'Val(16#c2ba#);

   -- Inverted_Question '¿'
   Inverted_Question                     : constant Char_Type
                                         := Char_Type'Val(16#c2bf#);

   -- Inverted_Negation '⌐'
   Inverted_Negation                     : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#8c90#));

   -- Negation '¬'
   Negation                              : constant Char_Type
                                         := Char_Type'Val(16#c2ac#);

   -- One_Half '½'
   One_Half                              : constant Char_Type
                                         := Char_Type'Val(16#c2bd#);

   -- One_Forth '¼'
   One_Forth                             : constant Char_Type
                                         := Char_Type'Val(16#c2bc#);

   -- Inverted_Exclamation '¡'
   Inverted_Exclamation                  : constant Char_Type
                                         := Char_Type'Val(16#c2a1#);

   -- Left_Angle_Quotation '«'
   Left_Angle_Quotation                  : constant Char_Type
                                         := Char_Type'Val(16#c2ab#);

   -- Right_Angle_Quotation '»'
   Right_Angle_Quotation                 : constant Char_Type
                                         := Char_Type'Val(16#c2bb#);

   -- D_Block_Dark '░'
   D_Block_Dark                          : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#9691#));

   -- D_Block_Normal '▒'
   D_Block_Normal                        : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#9692#));

   -- D_Block_Dim '▓'
   D_Block_Dim                           : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#9693#));

   -- D_Vertical '│'
   D_Vertical                            : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#9482#));

   -- D_Vertical_Middle_Left '┤'
   D_Vertical_Middle_Left                : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#94a4#));

   -- D_Vertical_Double_Middle_Left '╡'
   D_Vertical_Double_Middle_Left         : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#95a1#));

   -- D_Double_Vertical_Middle_Left '╢'
   D_Double_Vertical_Middle_Left         : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#95a2#));

   -- D_Top_Right_Corner_Double_Down '╖'
   D_Top_Right_Corner_Double_Down        : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#9596#));

   -- D_Top_Right_Corner_Double_Top '╕'
   D_Top_Right_Corner_Double_Top         : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#9595#));

   -- D_Double_Vertical_Double_Middle_Left '╣'
   D_Double_Vertical_Double_Middle_Left  : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#95a3#));

   -- D_Double_Vertical '║'
   D_Double_Vertical                     : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#9591#));

   -- D_Double_Top_Right_Corner '╗'
   D_Double_Top_Right_Corner             : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#9597#));

   -- D_Double_Bottom_Right_Corner '╝'
   D_Double_Bottom_Right_Corner          : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#959d#));

   -- D_Bottom_Right_Corner_Double_Right '╜'
   D_Bottom_Right_Corner_Double_Right    : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#959c#));

   -- D_Bottom_Right_Corner_Double_Bottom '╛'
   D_Bottom_Right_Corner_Double_Bottom   : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#959b#));

   -- D_Top_Right_Corner '┐'
   D_Top_Right_Corner                    : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#9490#));

   -- D_Bottom_Left_Corner '└'
   D_Bottom_Left_Corner                  : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#9494#));

   -- D_Horizontal_Middle_Up '┴'
   D_Horizontal_Middle_Up                : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#94b4#));

   -- D_Horizontal_Middle_Down '┬'
   D_Horizontal_Middle_Down              : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#94ac#));

   -- D_Vertical_Middle_Right '├'
   D_Vertical_Middle_Right               : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#949c#));

   -- D_Horizontal '─'
   D_Horizontal                          : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#9480#));

   -- D_Four_Directions '┼'
   D_Four_Directions                     : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#94bc#));

   -- D_Vertical_Double_Middle_Right '╞'
   D_Vertical_Double_Middle_Right        : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#959e#));

   -- D_Double_Vertical_Middle_Right '╟'
   D_Double_Vertical_Middle_Right        : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#959f#));

   -- D_Double_Bottom_Left_Corner '╚'
   D_Double_Bottom_Left_Corner           : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#959a#));

   -- D_Double_Top_Left_Corner '╔'
   D_Double_Top_Left_Corner              : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#9594#));

   -- D_Double_Horizontal_Double_Middle_Up '╩'
   D_Double_Horizontal_Double_Middle_Up  : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#95a9#));

   -- D_Double_Horizontal_Double_Middle_Down '╦'
   D_Double_Horizontal_Double_Middle_Down: constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#95a6#));

   -- D_Double_Vertical_Double_Middle_Right '╠'
   D_Double_Vertical_Double_Middle_Right : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#95a0#));

   -- D_Double_Horizontal '═'
   D_Double_Horizontal                   : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#9590#));

   -- D_Double_Four_Directions '╬'
   D_Double_Four_Directions              : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#95ac#));

   -- D_Double_Horizontal_Middle_Up '╧'
   D_Double_Horizontal_Middle_Up         : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#95a7#));

   -- D_Horizontal_Double_Middle_Up '╨'
   D_Horizontal_Double_Middle_Up         : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#95a8#));

   -- D_Double_Horizontal_Middle_Down '╤'
   D_Double_Horizontal_Middle_Down       : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#95a4#));

   -- D_Horizontal_Double_Middle_Down '╥'
   D_Horizontal_Double_Middle_Down       : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#95a5#));

   -- D_Bottom_Left_Corner_Double_Left '╙'
   D_Bottom_Left_Corner_Double_Left      : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#9599#));

   -- D_Bottom_Left_Corner_Double_Bottom '╘'
   D_Bottom_Left_Corner_Double_Bottom    : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#9598#));

   -- D_Top_Left_Corner_Double_Top '╒'
   D_Top_Left_Corner_Double_Top          : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#9592#));

   -- D_Top_Left_Corner_Double_Left '╓'
   D_Top_Left_Corner_Double_Left         : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#9593#));

   -- D_Double_Vertical_Middle_Horizontal '╫'
   D_Double_Vertical_Middle_Horizontal   : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#95ab#));

   -- D_Double_Horizontal_Middle_Vertical '╪'
   D_Double_Horizontal_Middle_Vertical   : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#95aa#));

   -- D_Bottom_Right_Corner '┘'
   D_Bottom_Right_Corner                 : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#9498#));

   -- D_Top_Left_Corner '┌'
   D_Top_Left_Corner                     : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#948c#));

   -- D_Block '█'
   D_Block                               : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#9688#));

   -- D_Half_Down_Block '▄'
   D_Half_Down_Block                     : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#9684#));

   -- D_Half_Left_Block '▌'
   D_Half_Left_Block                     : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#968c#));

   -- D_Half_Right_Block '▐'
   D_Half_Right_Block                    : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#9690#));

   -- D_Half_Up_Block '▀'
   D_Half_Up_Block                       : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#9680#));

   -- LC_Alpha 'α'
   LC_Alpha                              : constant Char_Type
                                         := Char_Type'Val(16#ceb1#);

   -- LC_Beta 'ß'
   LC_Beta                               : constant Char_Type
                                         := Char_Type'Val(16#c39f#);

   -- UC_Gamma 'Γ'
   UC_Gamma                              : constant Char_Type
                                         := Char_Type'Val(16#ce93#);

   -- LC_Pi 'π'
   LC_Pi                                 : constant Char_Type
                                         := Char_Type'Val(16#cf80#);

   -- UC_Sigma 'Σ'
   UC_Sigma                              : constant Char_Type
                                         := Char_Type'Val(16#cea3#);

   -- LC_Sigma 'σ'
   LC_Sigma                              : constant Char_Type
                                         := Char_Type'Val(16#cf83#);

   -- LC_Mu 'µ'
   LC_Mu                                 : constant Char_Type
                                         := Char_Type'Val(16#c2b5#);

   -- LC_Tau 'τ'
   LC_Tau                                : constant Char_Type
                                         := Char_Type'Val(16#cf84#);

   -- LC_Theta 'Φ'
   LC_Theta                              : constant Char_Type
                                         := Char_Type'Val(16#cea6#);

   -- UC_Theta 'Θ'
   UC_Theta                              : constant Char_Type
                                         := Char_Type'Val(16#ce98#);

   -- UC_Omega 'Ω'
   UC_Omega                              : constant Char_Type
                                         := Char_Type'Val(16#cea9#);

   -- LC_Delta 'δ'
   LC_Delta                              : constant Char_Type
                                         := Char_Type'Val(16#ceb4#);

   -- Infinity '∞'
   Infinity                              : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#889e#));

   -- LC_Phy 'φ'
   LC_Phy                                : constant Char_Type
                                         := Char_Type'Val(16#cf86#);

   -- LC_Epsilon 'ε'
   LC_Epsilon                            : constant Char_Type
                                         := Char_Type'Val(16#ceb5#);

   -- Set_Intersection '∩'
   Set_Intersection                      : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#88a9#));

   -- Congruent '≡'
   Congruent                             : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#89a1#));

   -- Plus_Minus '±'
   Plus_Minus                            : constant Char_Type
                                         := Char_Type'Val(16#c2b1#);

   -- Greater_Or_Equal '≥'
   Greater_Or_Equal                      : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#89a5#));

   -- Lower_Or_Equal '≤'
   Lower_Or_Equal                        : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#89a4#));

   -- Top_Integral '⌠'
   Top_Integral                          : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#8ca0#));

   -- Low_Integral '⌡'
   Low_Integral                          : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#8ca1#));

   -- Division '÷'
   Division                              : constant Char_Type
                                         := Char_Type'Val(16#c3b7#);

   -- Approx '≈'
   Approx                                : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#8988#));

   -- Empty_Circle '°'
   Empty_Circle                          : constant Char_Type
                                         := Char_Type'Val(16#c2b0#);

   -- Filled_Circle '∙'
   Filled_Circle                         : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#8899#));

   -- Dot '·'
   Dot                                   : constant Char_Type
                                         := Char_Type'Val(16#c2b7#);

   -- Square_Root '√'
   Square_Root                           : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#889a#));

   -- Raised_N 'ⁿ'
   Raised_N                              : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#81bf#));

   -- Raised_2 '²'
   Raised_2                              : constant Char_Type
                                         := Char_Type'Val(16#c2b2#);

   -- Filled_Square '■'
   Filled_Square                         : constant Str_Type
                                         := (1 => Char_Type'Val(16#00e2#),
                                             2 => Char_Type'Val(16#96a0#));


end Encoding;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
