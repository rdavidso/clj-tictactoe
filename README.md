# tictactoe

This is a clojure library to play a perfect AI game of Tic Tac Toe

## Usage

Use `lein run n` to play N games of the AI versus random moves.  If you would like to specify which move function to use, you may add `ai` or `random` to the end: `lein run n ai` will play N games of AI vs AI, which should all result in draws.  Any other text, or lack of defaults to random moves for the opponent.

Use `lein trampoline run` to do user input against the computer.  Enter moves in the form of y,x starting at 0,0 in the upper left corner.  Starting player is random but you are always :o and computer is :x.

## License

Copyright © 2015 

Distributed under the GNU GPLv3 License
