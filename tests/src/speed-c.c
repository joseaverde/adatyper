#include <stdio.h>
#include <sys/ioctl.h>

int main() {
   system("stty -echo");
   system("tput civis");

   struct winsize ws;
   ioctl(1, TIOCGWINSZ, &ws);
   const int width = ws.ws_col;
   const int height = ws.ws_row;

   for(char c = 1; c <= 8; c++) {
      for(int y = 0; y < height; y++) {
         printf("\033[%d;%dH", y + 1, 1);
         for(int x = 0; x < width; x++) {
            printf("\033[4%cm ", c + '0');
         }
      }
   }

   printf("\033[%d;%dH", height, width);

   system("stty echo");
   system("tput cnorm");
}
