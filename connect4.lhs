G52AFP Coursework 1 - Connect Four Game
         Shiqi XIN
         scysx1@nottingham.ac.uk

Library file
-------------
Please note: This program imports the random package,
which requires the random module to be installed on the computer.

> import Data.Char
> import Data.List
> import Data.String
> import System.IO
> import System.IO.Unsafe
> import System.Random


Basic definitions
-----------------

> rows :: Int
> rows = 6

> cols :: Int
> cols = 7

> win :: Int
> win = 4

> depth :: Int
> depth = 6


Basic declarations
------------------

> type Board = [Row]

> type Row = [Player]

> data Player = O | B | X
>               deriving (Ord, Eq, Show)

> data Tree a = Node a [Tree a]


Show boards and players
-----------------------

> showBoard :: Board -> IO ()
> showBoard b = putStrLn ("\n" ++ unlines (map showRow b ++ [line] ++ [nums]))
>               where
>                  showRow = map showPlayer
>                  line    = replicate cols '-'
>                  nums    = take cols ['0'..]

> showPlayer :: Player -> Char
> showPlayer O = 'O'
> showPlayer B = '.'
> showPlayer X = 'X'


Empty board
-----------
Use the replicate function to build an empty board with a size of rows * cols.

> initBoard :: Board
> initBoard = replicate rows (replicate cols B)


Count the number of players p on a given board
----------------------------------------------
Use recursion to calculate the sum of the number of players p in each row on the board.

> count :: Player -> Board -> Int
> count p [] = 0
> count p (xs:xss) = length (filter (== p) xs) + count p xss


Determine whose turn it is based on the number of players on the board
----------------------------------------------------------------------
If the number of player X on the board is greater,
it is player O's turn, otherwise it is player X's turn.

> turn :: Board -> Player
> turn b = if count O b <= count X b then O else X


Determine whether there are 'win' players p connected in a row in a given row
-----------------------------------------------------------------------------
Check whether all the first 'win' players in a row are player p, if not,
remove the first one and continue until it is detected or the length of the row is less than 'win'.

> hasRow :: Player -> Row -> Bool
> hasRow p r | length r < win                = False
>            | take win r == replicate win p = True
>            | otherwise                     = hasRow p (drop 1 r)


Append the elements in the row to the corresponding elements in the board
-------------------------------------------------------------------------
This function is an auxiliary function of the 'generate' function,
which is used to append each element in a row to the end of the corresponding row in the board.
If the length of the board is insufficient,
each remaining element in the row will be treated as a separate row.

> affix :: Row -> Board -> Board
> affix [] b = b
> affix (x:xs) [] = [x] : affix xs []
> affix (x:xs) (r:rs) = (r ++ [x]) : affix xs rs


Take the first element of the row as a separate row, and then combine the remaining part with the board
-------------------------------------------------------------------------------------------------------
This function is an auxiliary function of the 'rotate' function.
It takes the first element in the row as a separate row and combines
the remaining elements with the corresponding row in the board with the 'affix' function.

> generate :: Row -> Board -> Board
> generate r b = [head r] : affix (tail r) b


Regroup a board into a new board with each diagonal as a row
------------------------------------------------------------
This function starts from the bottom row of the board
and uses the foldr function to continuously append the diagonal elements
of the upper row to the corresponding previously formed rows.
More specifically, this function takes out the first element in the row
as a separate element each time, and attaches the remaining elements
of the row to the previously formed board using the'generate' function,
and finally forms a new board which uses the diagnoses of the original board as rows.
The left side of ++ calculates the case of tilting to the right,
and the right side of ++ calculates the case of tilting to the left.

> rotate :: Board -> Board
> rotate b = (foldr generate [] b) ++ (foldr generate [] (reverse b))


Determine whether a given player wins or not
--------------------------------------------
By detecting each row, each column, and each diagonal to determine whether player p wins or not.

> hasWon :: Player -> Board -> Bool
> hasWon p b = any row b || any row (transpose b) || any row (rotate b)
>              where row = hasRow p


Determine whether the game is a draw
------------------------------------
Judge whether the game is a draw by judging whether the first row of the board contains empty spaces.

> hasDraw :: Board -> Bool
> hasDraw b = not (elem B (b !! 0))


Find the number of rows reached by the player while moving in a given column
----------------------------------------------------------------------------
Judge the number of rows reached in the column by counting the number of empty positions in this column.

> findRow :: Int -> Board -> Int
> findRow a b = (length (elemIndices B ((transpose b) !! a))) - 1


Replace the element at the specified position in the row with the player p
--------------------------------------------------------------------------
This function is an auxiliary function of the 'move' function.

> moveRow :: Player -> Int -> Row -> Row
> moveRow p a xs = take a xs ++ [p] ++ drop (a + 1) xs


Replace the element at the specified position in the board with the player p
----------------------------------------------------------------------------
Move by replacing the new row generated by the 'moveRow' function into the original board.

> move :: Player -> Int -> Board -> Board
> move p a b = take r b ++ [moveRow p a (b !! r)] ++ drop (r + 1) b
>              where r = findRow a b


Determine whether the given column is full
------------------------------------------
Determine whether the column is full by counting the number of empty positions in this column.

> notExceed :: Int -> Board -> Bool
> notExceed a b = ([length (filter (== B) xs) | xs <- transpose b] !! a) /= 0


Determine whether the input is less than 0 or out of the column
---------------------------------------------------------------

> valid :: [Char] -> Board -> Bool
> valid xs b = elem (read xs) [0..(cols - 1)] && notExceed (read xs) b


Read a number and determine whether it is valid
-----------------------------------------------
Read the input, judge whether the input is empty,
whether it is a number and whether it is within the valid range,
if it is, then return the number, otherwise it prompts to re-enter.

> getNum :: Board -> IO Int
> getNum b = do xs <- getLine
>               if xs /= "" && all isDigit xs && valid xs b then
>                  return (read xs)
>               else
>                 do putStr "Invalid input, please try again: "
>                    getNum b


Return a tree generated after a node is expanded according to the given parameters
----------------------------------------------------------------------------------
This function is generated to avoid the 'expand' function being too verbose.
It generates a new tree whose depth is reduced by one compared with the original tree,
and the node is the original tree node that moves a player.

> nextBoard :: Int -> Player -> Board -> Int -> Tree Board
> nextBoard d p b a = expand (d - 1) (turn (move p a b)) (move p a b)


Expand the given board to a tree of a specified depth
-----------------------------------------------------
Continue expand the tree by moving the current board (node) one step
to all possible boards until there is a winner or a draw or the depth is reached.

> expand :: Int -> Player -> Board -> Tree Board
> expand d p b | d <= 0 || hasWon p b || hasDraw b = Node b []
>              | otherwise                         = Node b (map (nextBoard d p b) xs)
>                where xs = [a | a <- [0..(cols - 1)], notExceed a b]


Implement minimax algorithm
---------------------------
Use recursion to build the tree.
The leaf of this tree is a tuple composed of the winning player or B
and the current board, and each other node is a tuple composed of
the maximum/minimum player among its child nodes and the current board,
which is selected according to the current turn.

> minimax :: Tree Board -> Tree (Player, Board)
> minimax (Node b []) | hasWon O b  = Node (O, b) []
>                     | hasWon X b  = Node (X, b) []
>                     | otherwise   = Node (B, b) []
> minimax (Node b ts) | turn b == O = Node (minimum ps, b) (map minimax ts)
>                     | otherwise   = Node (maximum ps, b) (map minimax ts)
>                                     where
>                                       ps = [p | Node (p, _) _ <- (map minimax ts)]


Randomly select one item from the given list
--------------------------------------------
Generate a random number within the length of the list and take this list item.

> pickRandom :: [a] -> a
> pickRandom xs = xs !! unsafePerformIO (randomRIO (0, (length xs - 1)))


Find the optimal next step from the given tree
----------------------------------------------
Obtain a board from the tree generated by the minimax algorithm,
which is randomly select a board from the nodes in the second layer of the tree,
whose player is the same as the first layer's.

> selectMove :: Tree (Player, Board) -> Board
> selectMove (Node (p1, _) ts) = pickRandom [b2 | Node (p2, b2) _ <- ts, p2 == p1]


Keep running the game until the termination conditions are met
--------------------------------------------------------------
If one of the player wins or the game is a draw,
the corresponding prompt will be shown.
If the next turn is a human turn,
the input will be obtained and the game will continue according to the input step.
If the next turn is a computer turn,
the next step will be obtained through AI and the game will continue according to that.

> start :: Player -> Board -> IO ()
> start p b | hasWon O b    = do showBoard b
>                                putStrLn "Player O wins!"
>           | hasWon X b    = do showBoard b
>                                putStrLn "Player X wins!"
>           | hasDraw b     = do showBoard b
>                                putStrLn "Draw!"
>           | (turn b) == O = do showBoard b
>                                putStr "Player O enter your move: "
>                                a <- getNum b
>                                start (turn (move p a b)) (move p a b)
>           | otherwise     = do showBoard b
>                                putStrLn "Player X is thinking..."
>                                start (turn nb) nb
>                                where
>                                  nb = selectMove (minimax (expand depth p b))


Start the game
--------------

> main :: IO ()
> main = do hSetBuffering stdout NoBuffering
>           start O initBoard