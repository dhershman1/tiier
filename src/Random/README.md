# Probability Engine

Tiier's probability engine is used mainly for building out the play boards through out the world.

It's currently heavily WIP mode for this piece of the game.

Ideally it will:

- Properly know how to build out boards fully
- Properly place events through out the map
- Properly place treasures/loot through out the map
- Build out full fledge rooms as well as environments like water/lava
- Handle placing traps fairly with dynamic environment changing events

## How

There are a few strats on how this should work.

- Run multiple iterations over the map each time learning and placing new pieces
- Using the power of Dijkstra Maps algorithms

I want to probably do a mixture but in the end I think the Dijkstra Maps is what we are shooting for. Giving us a lot of benefit in the end.

For now we are building out the building blocks this all sits on. These building blocks will expand and grow into the major functionality of this engine.

## Foundation

The foundation (as stated above) is the bare bones right now.

The easiest approach in my opinion here, might be to rely on the `Random.uniform` functionality, and generate bits and remove them from a list when we don't want to generate anymore of that tile.

Another approach is plopping `cores` like `Room Cores` which signify a special location and build different sized room off that core. Now I'm not sure how this fleshes out yet especially since the rooms can vary by a lot.

It may also be worth building them in the sense that the edges are all walls, with 1 enterance and 1 exit. And then build out the terrain within the giant "room" instead of making the board a mini location of rooms, it would be more of the board itself is the room. This fits my old perspective of making "dungeons" or areas larger by making them a bunch of these rooms instead.
