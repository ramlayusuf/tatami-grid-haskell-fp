# tatami-grid-haskell-fp

A functional-programming implementation of a tatami-tiling grid system in Haskell.
The project defines a pointer-based grid structure and provides operations to:
Move a pointer around the grid
Modify cell values
Place 2×1 tatami tiles vertically or horizontally
Recursively cover the entire grid using tatami tiles if possible

Grid Types:
Grid a - a 2D structure storing values of any type

GridWithAPointer a - a grid that tracks:
all rows above the pointer as a grid
the value under the pointer
the row segment to the left and right as lists
all rows below as a grid
This representation allows  movement without storing explicit coordinates

Tatami tiles are 2×1 dominos, numbered sequentially.
The cover function attempts to fill an initially empty grid with sequential tatami tiles using a frame-by-frame recursive strategy:
Moves the pointer to the top-left
Computes inner “frames”
Fills each frame using a deterministic pattern
Ensures grids with an odd number of cells are rejected
