# Map

This file set is responsible for building out each map within Tiier

The process runs as such:

1. The map is planned out based on size
2. It is then filled with all "floor" or "water" tiles
3. The starter room is added (where the player spawns)
4. The next pass over adds the layout into the map with walls
5. Pass #3 adds terrain to be filled in such as water, forests, etc.
    - This is based on a probability engine the AI uses
6. The AI is then instructed to connect the rooms together in some manner AND to ensure there is a way to get to the exit
7. The last pass through is what generates the treasure, events, traps, and monsters on the map
    - This is also driven by the AI storyteller

The above process is driven by an AI like storyteller or "DM" you can read more on that in the AI folder.

This process is randomized and saved to a seed. This seed is then reused for your continuation of the game during **this** session. Once you leave and come back to play later, all of your seeds will be regenerated and all new maps created for you. Think of `Diablo` when you reload the game all the dungeons sort of change to a degree.

Currently Steps 1, 2, and 3 are working.

## TODO

- Create the pass that starts building out the map layout
- Build out the probabiliy engine that the AI uses to place terrain
- Use the Pathfinding algorithm to connect each room and ensure a connection to the end room
- Create the pass over which populates the map board

