
Note to self:

Playing with the Map (now board) in build out a proper grid and playable board

we will need to generate a cell with all the specs and stats we expect, for each bit of the grid.

We want to do some probability so that the terrain is varied as it builds out but also contained (water is within water) etc.

Though we may need to use a different formula for building out walls/paths/rooms since these need to be more defined for player movement and the like. These are the pieces I am currently struggling with the most. Once we get past that it's more of the look and feel rather than actually generating things.

Basic information about a map will be predetermined (unless we build a random map generator sandbox later which is cool but different anyway) within a database somewhere.

Generator Idea:

- First Pass builds the pass grid foundation
- Second Pass places `Room Cores` around the grid
- Third Pass Builds out varrying rooms of size
- Fourth Pass Builds terrain around this rooms
- Fifth Pass populates the board

These could be merged in some manner like the first and second pass. I think we need to get the room layou done and in place so those are setup. Though I feel like the more passes the slower the map loads in.

Maybe each piece of terrain can have a Core of some kind like water (lakes), forests, etc. That would build a varrying size of that tile. Similar to how the rooms construct. Though at the sametime maybe it's a bad idea?

The hardest part will be making sure it's a playable map. We can throw terrain around willy nilly but functionality will need to take into consideration the ability to finish and play through a board.
