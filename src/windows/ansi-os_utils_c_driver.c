/*****************************************************************************\
 *                                                                           * 
 *              A N S I - O S _ U T I L S _ C _ D R I V E R . C              * 
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
 * This file is used to work with some functions from <windows.h>, I'm not   *
 * going to do it directly from Ada because it's hard to find out how a type *
 * is defined to make the proper interface so I'm going to create a kind of  *
 * C Driver (Glue Code) to interface with the <windows.h> library from the   *
 * Windows API.                                                              *
 *                                                                           *
 * All the functions will be int and the variables will be out parameters.   *
 * If the function returns 0, then there wasn't any problem.                 *
\*                                                                           */

#include <stdio.h>
#include <windows.h>

#ifndef ENABLE_VIRTUAL_TERMINAL_PROCESSING
#define ENABLE_VIRTUAL_TERMINAL_PROCESSING 0x0004
#endif

static HANDLE stdoutHandle;
static DWORD outModeInit;

// This function sets up the console.
void setupConsole( void ) {
   DWORD outMode = 0;
   stdoutHandle = GetStdHandle(STD_OUTPUT_HANDLE);

   if ( stdoutHandle == INVALID_HANDLE_VALUE ) {
      exit( GetLastError() );
   }

   if ( ! GetConsoleMode( stdoutHandle, &outMode ) ) {
      exit( GetLastError() );
   }
   
   outModeInit = outMode;
   outMode |= ENABLE_VIRTUAL_TERMINAL_PROCESSING;

   if ( ! SetConsoleMode( stdoutHandle, outMode ) ) {
      exit( GetLastError() );
   }
}


// This function restores the console.
void restoreConsole( void ) {
   printf("\033[0m");
   if ( ! SetConsoleMode( stdoutHandle, outModeInit ) ) {
      exit( GetLastError() );
   }
}

// This function returns the number of rows and columns the terminal has
int getConsoleScreenSize (short *rows, short *cols) {
   // TODO: Finish this.
   return 1;
}



///\\\\\\\\\\\\\\\\\\\\\\\/////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\///
//\\\\\\\\\\\\\\\\\\\\\\\// E N D   O F   F I L E //\\\\\\\\\\\\\\\\\\\\\\\\\//
///\\\\\\\\\\\\\\\\\\\\\\\/////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\///
