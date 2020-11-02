OPTIMIZATIONS
=============
This file contains posible optimizations to different procedures.


## Surface Redering
Each surface contains a matrix (grid) of its size.
And each element contains a character and a format (Styles, Colours)

1. Reduce the number of elements updated with a tail with all changes performed into the Surface. Thus, the surface must be a private type so it can't be edited without registering the changes. This reduces the O(n^2) cost, to a cost equal to the changes and not the matrix itself.

2. Instead of using the normal Text_IO, use Sequential_IO or Streams to make it faster, also avoid the flushing of the screen until the screen is updated. Because the flushing operations and the printing operations themseveles cost a lot of time as seen in the speed test.

3. Keep track of the last style used when printing in the same line, so the program doesn't have to print the same colour and style for every element. Then, only the different ones will be edited. This reduces the number of calls to the output function, only doing it when needed. (I have to check it, but a think 3 comparisons and 3 conditional prints are way faster than 3 raw prints).

4. Change the algorithm to convert numbers into strings. Maybe it's faster to use a loop and using only character like: \033[9A\033[9A\033[3A than \033[21A. Or check the length of the number first.

5. When working with layers, all layers THAT HAVE BEEN EDITED will be merged together in the order they have been placed into the Main_Surface, reducing the number of changes. Having them be printed into the Main_Surface adds control and reduces the number of print calls.

6. When a surface is moved it no extra operations will be performed.
