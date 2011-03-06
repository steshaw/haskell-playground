module TicTacToe where

data Player = Player1 | Player2

type Winner = Maybe Player

data Board

data NonEmptyBoard

data EmptyBoard

data ActiveBoard = ActiveBoard

data UnsureBoard = UnsureBoard

data CompletedBoard

data Position

{-
move: takes a tic-tac-toe board and position and moves to that
position (if not occupied) returning a new board. This function can
only be called on a board that is in-play. Calling move on a game
board that is finished is a *compile-time type error*.
-}
move :: ActiveBoard -> Position -> UnsureBoard
move activeBoard pos = UnsureBoard

{-
whoWon: takes a tic-tac-toe board and returns the player that won
the game (or a draw if neither). This function can only be called on a
board that is finished. Calling move on a game board that is in-play
is a *compile-time type error*.
-}
whoWon :: CompletedBoard -> Winner
whoWon board = Nothing

{-
takeBack: takes either a finished board or a board in-play that has
had at least one move and returns a board in-play. It is a
compile-time type error to call this function on an empty board.
-}
takeBack :: NonEmptyBoard -> ActiveBoard -- or EmptyBoard...
takeBack board = ActiveBoard

{-
playerAt: takes a tic-tac-toe board and position and returns the
(possible) player at a given position. This function works on any type
of board.
-}
playerAt :: Board -> Maybe Player
playerAt board = Nothing
