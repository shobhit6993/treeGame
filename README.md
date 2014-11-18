treeGame
========
----
### Running Instructions (for Linux)
    ghci --make game.hs
    ./game <playMode> [levelNo]

* playMode can be `1` (single-player version) or `2` (2-player version)
* levelNo can be any integer between `1` and `8` *(currently, there are 8 levels hard-coded in Levels.hs. More levels can be added.)*

Example command to play 1 player version (User vs Computer) for level=5

    ghci --make game.hs
    ./game 1 5
----
### Game Play

* If you are playing the 1 player version, user will be Player#1 and computer will be Player#2.
* Trees will be displayed along with their treeNo.
* Each edge will have two nodes - sourceNode (lower one) and destinationNode (upper one)
* For a move, you need to first enter the treeNo, then sourceNode no., and then destination node no.
* For example, if you want to remove edge between node 4 and 5 of tree 2,
	1. Enter Tree number and then press **Right Arrow Key**
	2. Enter souce node no. and then press **Right Arrow Key**
	3. Enter destination node no. and then press **Right Arrow Key**
	4. Press **Right Arrow Key** to confirm move
	5. Now player 2's turn
	6. If you enter an invalid move, then the game asks you to enter the move again, till you give a valid move

---
### Game Rules

* This game is a variant of the [Hackenbush](http://en.wikipedia.org/wiki/Hackenbush) game.
* 2 players alternate turns.
* Player 1 can remove either one `Red` edge or one `Blue` edge
* Player 2 can remove either one `Green` edge or one `Blue` edge
* When an edge `E` is removed, the its entire subtree (including itself) is pruned.
* The player which plays the last move wins

---
### System Requirements
* haskell-platform
* OpenGL for drawing purposes, and GTK2 for UI.
* For Ubuntu or any other Debian distributed OS like Elementary, follow these steps to install OpenGL + GTK2

----
    sudo apt-get install haskell-platform
    cabal update
    sudo apt-get install libgtkglext1-dev
    cabal install gtk2hs-buildtools
    export PATH=$HOME/.cabal/bin:$PATH
    cabal install gtk opengl gtkglext

    
