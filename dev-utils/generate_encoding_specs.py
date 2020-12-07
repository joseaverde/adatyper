#!/usr/bin/env python3
# *-* coding=utf-8 *-*
#=============================================================================#
#                                                                             #
#             G E N E R A T E _ E N C O D I N G _ S P E C S . P Y             #
#                                                                             #
#                               A D A T Y P E R                               #
#                                                                             #
#                                 S C R I P T                                 #
#                                                                             #
#-----------------------------------------------------------------------------#
#      Copyright (c) 2020 José Antonio Verde Jiménez All Rights Reserved      #
#-----------------------------------------------------------------------------#
#  This file is part of adatyper.                                             #
#                                                                             #
#  This program is free software:  you  can redistribute it and/or modify it  #
#  under  the terms  of the  GNU  General License  as published by the  Free  #
#  Software  Foundation,  either  version 3  of  the  License,  or  (at your  #
#  opinion) any later version.                                                #
#                                                                             #
#  This  program  is distributed  in the  hope that  it will be  useful, but  #
#  WITHOUT   ANY   WARRANTY;   without   even  the   implied   warranty   of  #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General  #
#  Public License for more details.                                           #
#                                                                             #
#  You should have received  a copy of the  GNU General Public License along  #
#  with this program. If not, see <https://www.gnu.org/licenses/>.            #
#                                                                             #
#=============================================================================#

import os

encoding = {
    128: ["Ç", "UC_C_Cedilla"],
    129: ["ü", "LC_Dieresis"],
    130: ["é", "LC_E_Acute"],
    131: ["â", "LC_A_Circumflex"],
    132: ["ä", "LC_A_Dieresis"],
    133: ["à", "LC_A_Grave"],
    134: ["å", "LC_A_Ring"],
    135: ["ç", "LC_C_Cedilla"],
    136: ["ê", "LC_E_Circumflex"],
    137: ["ë", "LC_E_Dieresis"],
    138: ["è", "LC_E_Grage"],
    139: ["ï", "LC_I_Dieresis"],
    140: ["î", "LC_I_Circumflex"],
    141: ["ì", "LC_I_Grave"],
    142: ["Ä", "UC_A_Dieresis"],
    143: ["Å", "UC_A_Ring"],
    144: ["É", "UC_E_Acute"],
    145: ["æ", "LC_AE_Diphthong"],
    146: ["Æ", "UC_AE_Diphthong"],
    147: ["ô", "LC_O_Circumflex"],
    148: ["ö", "LC_O_Dieresis"],
    149: ["ò", "LC_O_Grave"],
    150: ["û", "LC_U_Circumflex"],
    151: ["ù", "LC_U_Grave"],
    152: ["ÿ", "LC_Y_Dieresis"],
    153: ["Ö", "UC_O_Dieresis"],
    154: ["Ü", "UC_U_Dieresis"],
    155: ["¢", "Cent_Sign"],
    156: ["£", "Pound_Sign"],
    157: ["¥", "Yen_Sign"],
    158: ["₧", "Something_1"],
    159: ["ƒ", "Forte"],
    160: ["á", "LC_A_Acute"],
    161: ["í", "LC_I_Acute"],
    162: ["ó", "LC_O_Acute"],
    163: ["ú", "LC_U_Acute"],
    164: ["ñ", "LC_N_Tilde"],
    165: ["Ñ", "UC_N_Tilde"],
    166: ["ª", "Feminine_Ordinal_Indicator"],
    167: ["º", "Masculine_Ordinal_Indicator"],
    168: ["¿", "Inverted_Question"],
    169: ["⌐", "Inverted_Negation"],
    170: ["¬", "Negation"],
    171: ["½", "One_Half"],
    172: ["¼", "One_Forth"],
    173: ["¡", "Inverted_Exclamation"],
    174: ["«", "Left_Angle_Quotation"],
    175: ["»", "Right_Angle_Quotation"],
    176: ["░", "D_Block_Dark"],
    177: ["▒", "D_Block_Normal"],
    178: ["▓", "D_Block_Dim"],
    179: ["│", "D_Vertical"],
    180: ["┤", "D_Vertical_Middle_Left"],
    181: ["╡", "D_Vertical_Double_Middle_Left"],
    182: ["╢", "D_Double_Vertical_Middle_Left"],
    183: ["╖", "D_Top_Right_Corner_Double_Down"],
    184: ["╕", "D_Top_Right_Corner_Double_Top"],
    185: ["╣", "D_Double_Vertical_Double_Middle_Left"],
    186: ["║", "D_Double_Vertical"],
    187: ["╗", "D_Double_Top_Right_Corner"],
    188: ["╝", "D_Double_Bottom_Right_Corner"],
    189: ["╜", "D_Bottom_Right_Corner_Double_Right"],
    190: ["╛", "D_Bottom_Right_Corner_Double_Bottom"],
    191: ["┐", "D_Top_Right_Corner"],
    192: ["└", "D_Bottom_Left_Corner"],
    193: ["┴", "D_Horizontal_Middle_Up"],
    194: ["┬", "D_Horizontal_Middle_Down"],
    195: ["├", "D_Vertical_Middle_Right"],
    196: ["─", "D_Horizontal"],
    197: ["┼", "D_Four_Directions"],
    198: ["╞", "D_Vertical_Double_Middle_Right"],
    199: ["╟", "D_Double_Vertical_Middle_Right"],
    200: ["╚", "D_Double_Bottom_Left_Corner"],
    201: ["╔", "D_Double_Top_Left_Corner"],
    202: ["╩", "D_Double_Horizontal_Double_Middle_Up"],
    203: ["╦", "D_Double_Horizontal_Double_Middle_Down"],
    204: ["╠", "D_Double_Vertical_Double_Middle_Right"],
    205: ["═", "D_Double_Horizontal"],
    206: ["╬", "D_Double_Four_Directions"],
    207: ["╧", "D_Double_Horizontal_Middle_Up"],
    208: ["╨", "D_Horizontal_Double_Middle_Up"],
    209: ["╤", "D_Double_Horizontal_Middle_Down"],
    210: ["╥", "D_Horizontal_Double_Middle_Down"],
    211: ["╙", "D_Bottom_Left_Corner_Double_Left"],
    212: ["╘", "D_Bottom_Left_Corner_Double_Bottom"],
    213: ["╒", "D_Top_Left_Corner_Double_Top"],
    214: ["╓", "D_Top_Left_Corner_Double_Left"],
    215: ["╫", "D_Double_Vertical_Middle_Horizontal"],
    216: ["╪", "D_Double_Horizontal_Middle_Vertical"],
    217: ["┘", "D_Bottom_Right_Corner"],
    218: ["┌", "D_Top_Left_Corner"],
    219: ["█", "D_Block"],
    220: ["▄", "D_Half_Down_Block"],
    221: ["▌", "D_Half_Left_Block"],
    222: ["▐", "D_Half_Right_Block"],
    223: ["▀", "D_Half_Up_Block"],
    224: ["α", "LC_Alpha"],
    225: ["ß", "LC_Beta"],
    226: ["Γ", "UC_Gamma"],
    227: ["π", "LC_Pi"],
    228: ["Σ", "UC_Sigma"],
    229: ["σ", "LC_Sigma"],
    230: ["µ", "LC_Mu"],
    231: ["τ", "LC_Tau"],
    232: ["Φ", "LC_Theta"],
    233: ["Θ", "UC_Theta"],
    234: ["Ω", "UC_Omega"],
    235: ["δ", "LC_Delta"],
    236: ["∞", "Infinity"],
    237: ["φ", "LC_Phy"],
    238: ["ε", "LC_Epsilon"],
    239: ["∩", "Set_Intersection"],
    240: ["≡", "Congruent"],
    241: ["±", "Plus_Minus"],
    242: ["≥", "Greater_Or_Equal"],
    243: ["≤", "Lower_Or_Equal"],
    244: ["⌠", "Top_Integral"],
    245: ["⌡", "Low_Integral"],
    246: ["÷", "Division"],
    247: ["≈", "Approx"],
    248: ["°", "Empty_Circle"],
    249: ["∙", "Filled_Circle"],
    250: ["·", "Dot"],
    251: ["√", "Square_Root"],
    252: ["ⁿ", "Raised_N"],
    253: ["²", "Raised_2"],
    254: ["■", "Filled_Square"]
}


def main():
    licence = """
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
    """[1:].rstrip(" ")

    summary = """
--
-- @summary
--
    """[1:].rstrip(" ")

    procedures = """

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

    """[1:].rstrip(" ")

    description = """
--
-- @description
--
    """[1:].rstrip(" ")

    eof = """
---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
    """.rstrip(" ")

    struct = "{license}\n"        \
             "with Toolbox;\n"     \
             "use Toolbox;\n\n"     \
             "{summary}"             \
             "{description}"          \
             "package Encoding is\n"   \
             "\n{procedures}\n"         \
             "\n{spec}\n"                \
             "end Encoding;\n\n"          \
             "{eof}"

    spaces = 1
    for pos in encoding.keys():
        s = len(encoding[pos][1])
        if s > spaces:
            spaces = s

    linux_spec = ""
    windows_spec = ""

    fmt = "   -- {name} '{char}'\n" \
          "   {name}{spaces}: constant Char_Type\n" \
          "   %s:= {val};\n\n" % (spaces * " ")
    fmt2 = "   -- {name} '{char}'\n" \
           "   {name}{spaces}: constant Str_Type\n" \
           "   %s:= {val};\n\n" % (spaces * " ")

    def to_hex(char):
        data = char.encode("utf-8")
        s = ""
        for d in data:
            _ = hex(d)[2:]
            _ = ("" if len(_) == 2 else "0") + _
            s += _
        return s

    to_hex_2 = lambda n, b=4: (b - len(hex(n)) + 2) * '0' + hex(n)[2:].upper()
    for pos in encoding.keys():
        val = encoding[pos]
        _ = to_hex(val[0])
        if len(_) == 4:
            linux_spec += fmt.format(name   = val[1],
                                     char   = val[0],
                                     spaces = " " * (spaces - len(val[1])),
                                     val    = "Char_Type'Val(16#%s#)" % _)
        else:
            _ = (8 - len(_)) * '0' + _
            linux_spec += fmt2.format(name   = val[1],
                                      char   = val[0],
                                      spaces = " " * (spaces - len(val[1])),
                                      val    = ("(1 => " +
                                                "Char_Type'Val(16#%s#)," +
                                                "\n" + spaces * " " + "      "+
                                                " 2 => "+
                                                "Char_Type'Val(16#%s#))")%
                                               (_[:4], _[-4:]))
        windows_spec += fmt.format(name   = val[1],
                                   char   = val[0],
                                   spaces = " " * (spaces - len(val[1])),
                                   val    = "Char_Type'Val(16#%s#)" %
                                             to_hex_2(pos))

    linux_file = open("obj/encoding:utf-8.ads", 'w')
    linux_file.write(struct.format(license     = licence,
                                   summary     = summary,
                                   description = description,
                                   procedures  = procedures,
                                   spec        = linux_spec,
                                   eof         = eof))
    linux_file.close()

    windows_file = open("obj/encoding:ascii.ads", 'w')
    windows_file.write(struct.format(license     = licence,
                                     summary     = summary,
                                     description = description,
                                     procedures  = procedures,
                                     spec        = windows_spec,
                                     eof         = eof))
    windows_file.close()

if __name__ == '__main__':
    if not os.path.exists("obj"):
        os.mkdir("obj")
    main()

###=======================-------------------------=========================###
##=======================-- E N D   O F   F I L E --=========================##
###=======================-------------------------=========================###
