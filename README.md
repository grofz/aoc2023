# AoC2023
My cool new project!

## Day 01: Trebuchet?!
Recognize "1", "2", ... and "one", "two", ... at the beginning and at the end
of the line. Difficult as the test case for Part 2 did not contain a case as
for example "oneight". Using "scan" and "index" intrinsics.

## Day 02: Cube Conundrum
Parse the clear syntax to the array, Search for maximum values in this array.
Using "split" function.

## Day 03: Gear Ratios
Find text labels in a 2D-array of characters, then search arouond this label
for the presence of particular characters.

## Day 04: Scratchcards
Parse a line of numbers into a pair of arrays. Compare the two arrays.

## Day 05: If You Give A Seed A Fertilizer
Map an interval of numbers to another interval using a list of rules, find the minimum value of the new interval. Part 1 to part 2 transition was from processing several points to maping a huge number of points in interval sets.  

## Day 06: Wait For It
Optimize the time needed to "charge" the boat. Part 2 solved by solving the quadratic formula, but brute-force would also work.

## Day 07: Camel Cards
Identify the strength of a hand of cards (flush, pairs, etc.), sort the cards according to their strength. Using OOP. Part 2 introduces the Joker cards and modifies the sorting order.

## Day 08: Haunted Wasteland
Traverse a binary graph until finding a particular node. Part 2 requires one to figure out that the input graph allows to use a least common multiplier approach (all paths are circular)

## Day 09: Mirage Maintenance
Predict the next value in the pattern. Solved by using a recursion and making finite difference of an array of numbers. Part 2 is a trivial modification of Part 1 solution.

## Day 16: The Floor Will Be Lava
Track beams of light trough a maze of mirrors and splitters. Count the number of tiles energized by
the light. Using an array of beams as a queue of active beams and a subroutine to generate the
new beam(s) after one step. To prevent endless loop, a mask for each tile and direction is used,
the beam does not propagate if the mask is already true. Part 2 just loops over different initial
beam positions.

## Day 17: Clumsy Cruicible
Find a minuimum path of a carriage with minimum and maximum streak of direct path
through a 2D heat map. Using Djikstra with priority queue to obtain a fast
solution.

## Day 18: Lavaduct Lagoon
Similar to Day 10, use Shoelace's formula to calculate the area enclosed by a polygon.

## Day 20: Pulse propagation
Simulate a pulse through the network of modules. For Part 2, the size of four different cycles
must be found and LCM used to obtain the answer.

## Day 21: Step Counter
The most difficult problem so far. Count the number of cells that can be reached within
the set number of steps. For Part 2: Brute-forcing until a pattern is found, then using
similar technique as in Day 9 to predict the next number in the series.

## Day 22: Sand Slabs
Simulate the fall of 3D blocks of sand.

## Day 23: A long walk
Find the longest path in a labyrinth. Solved by reducing the number of vertices and then
recursion driven Deep-First-Search.