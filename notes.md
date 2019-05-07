
Note to self:

**Map size**: 50x40

Playing with the Map (now board) in build out a proper grid and playable board

we will need to generate a cell with all the specs and stats we expect, for each bit of the grid.

We want to do some probability so that the terrain is varied as it builds out but also contained (water is within water) etc.

Though we may need to use a different formula for building out walls/paths/rooms since these need to be more defined for player movement and the like. These are the pieces I am currently struggling with the most. Once we get past that it's more of the look and feel rather than actually generating things.

Basic information about a map will be predetermined (unless we build a random map generator sandbox later which is cool but different anyway) within a database somewhere.

It seems like `Random.step` is our answer I believe. But this should NOT happen per tile/cell that would be bad news. Instead shoot for Rows and then each id can be a list of `Seeds` which pertain to the rows within a `Board`

This would allow us to generate a random probability for tiles within a row. This may not be the 100% solution we are looking for, though it may be useful to depict how rooms/halls/etc could be generated.

**OR** We scrap that and use the map `Seed` idea still, since we can feed the map seed to pretty much all of the `Random.step` functions to generate the same results over and over!
