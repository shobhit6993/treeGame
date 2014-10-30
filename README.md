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

* This came is a variant of the [Hackenbush](http://en.wikipedia.org/wiki/Hackenbush) game.
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

    
