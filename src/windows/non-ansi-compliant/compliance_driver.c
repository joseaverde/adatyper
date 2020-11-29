/*****************************************************************************\
 *                                                                           * 
 *                      W I N D O W S - C O L O R S . C                      * 
 *                               W I N D O W S                               * 
 *                                                                           * 
 *                              A D A T Y P E R                              * 
 *                                                                           * 
 *                                D R I V E R                                * 
 *                                                                           * 
 *---------------------------------------------------------------------------* 
 *     Copyright (c) 2020 José Antonio Verde Jiménez All Rights Reserved     * 
 *---------------------------------------------------------------------------* 
 * This file is part of adatyper.                                            * 
 *                                                                           * 
 * This program is free software:  you  can redistribute it and/or modify it * 
 * under  the terms  of the  GNU  General License  as published by the  Free * 
 * Software  Foundation,  either  version 3  of  the  License,  or  (at your * 
 * opinion) any later version.                                               * 
 *                                                                           * 
 * This  program  is distributed  in the  hope that  it will be  useful, but * 
 * WITHOUT   ANY   WARRANTY;   without   even  the   implied   warranty   of * 
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General * 
 * Public License for more details.                                          * 
 *                                                                           * 
 * You should have received  a copy of the  GNU General Public License along * 
 * with this program. If not, see <https://www.gnu.org/licenses/>.           * 
 *                                                                           * 
\*****************************************************************************/

/*                                                                           *\
 * This is a driver to work with the windows API functions in Ada.           *
\*                                                                           */

#include <windows.h>

// This function sets the foreground and background colours and attributes to
// the windows console.
void setWindowsConsoleColorWithAttributes ( WORD Colors ) {
   SetConsoleTextAttribute ( GetStdHandle ( STD_OUTPUT_HANDLE ),
                             Colors );
}



///\\\\\\\\\\\\\\\\\\\\\\\/////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\///
//\\\\\\\\\\\\\\\\\\\\\\\// E N D   O F   F I L E //\\\\\\\\\\\\\\\\\\\\\\\\\//
///\\\\\\\\\\\\\\\\\\\\\\\/////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\///
