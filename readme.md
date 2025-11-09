# Pac-Man

## How to Play
Simply execute `cabal run` in the root of this project.
For extra options:
- The `/boards` folder holds custom boards (see [Creating a Custom Board](#creating-a-custom-board))

### Controls:
- Navigate around menus using `w` `s` and `enter`
- Control Pac-Man with `w` `a` `s` `d`
- Pause the game at any time with `p` (and save with `s`)
- Use `esc` at any point to exit the game
- You can 'queue' your next move, making it easier to navigate around. The red arrow around Pac-Man indicates what direction you will go to once it is possible.

## Saving game
To save a game, press `s` from the pause menu, a `gamesave.json` will appear in the root project.
To load this in, put it in the `/saves` folder (with any name you want) and load it in from the home screen by going to the saves menu (`l`) and pressing `enter` on the save file you want to continue playing from. The game will automatically resume after loading.

## Creating a Custom Board
To create a custom board, use the mapping provided below. Create a file and separate each tile with a space. You can also use the more user friendly [Board Designer](https://docs.google.com/spreadsheets/d/1d7oN5Gdvx4rYr7zIro5RU-HJ-WNQdoWsWn6A2U10TTM/edit?usp=sharing) from which you can simply copy and paste the board into an empty file. Put the file in `/boards` and load it from the home screen by pressing `s`. Warning: boards need at least 1 pellet, 1 player spawn and 1 ghost spawn tile. If a board does not show up, make sure all these are included and all rows are of the same length.

| **Tile**     | **Char** |
|--------------|----------|
| Wall         | W        |
| Empty        | E        |
| Pellet       | P        |
| Power Pellet | S        |
| Ghost Spawn  | G        |
| Ghost Exit   | H        |
| Player Spawn | X        |


## Extra features
- Dynamic resizing based on game screen size
- Automatic wrap-around detection for custom boards
- Custom boards
- Saving and loading a game
- Movement queueing for easier gameplay